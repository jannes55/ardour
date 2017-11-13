/*
 * Copyright (C) 2000 Paul Davis & Benno Senoner
 * Copyright (C) 2017 Robin Gareus <robin@gareus.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#ifndef random_access_ringbuffer_h
#define random_access_ringbuffer_h

#include <cstring>
#include <glibmm.h>

#include "pbd/libpbd_visibility.h"
#include "pbd/spinlock.h"

namespace PBD {

/* This Ringbuffer implements a 2 segment cache,
 * with absolute sample position intended for audio-playback.
 *
 * A writer thread fills the buffer sequentially
 * from a given start position.
 *
 * A reader thread can read data from any point, without
 * directly invalidating data in the buffer. The use-case
 * for this is to allow micro-seeks backwards.
 * The reader can read the same data more than once.
 *
 * The writer thread may block, the reader is lock-free
 * (except for a spin-lock).
 *
 * The read-pointer indicates the last position of the reader.
 * At instantiation time a "reservation" count is set.
 *
 * It is guarnteed that the writer will not overwrite "reservation"
 * number of entries before the reader-position. This allows
 * the reader to /rewind/ and re-read data.
 *
 * For non-linear playback (e.g. looping) a 2nd segment is
 * used. The ring-buffer may contain the end of
 * a loop-range and the start of a loop-range.
 *
 * before writing after a loop-point, the writer should
 * check if the data is already present (can_read()).
 * In case the complete loop fits into the RB it only needs to
 * be written once.
 *
 *
 * XXX XXX XXX
 * the read pointer always moves forward (and wraps around)
 * moving over a segment-boundary invalidates it.
 * (once read-pointer - reservation >= segment-end)
 *
 * -> active segment.
 *
 *  edge-case: shorten loop while buffer is full with current data
 *  (no space to write a new segment)
 *
 *  if there are 2 segments, the writer can only append to the later one,
 *  the reader can only invalidate the older segment.
 *
 *  .. moving regions..
 */

template<class T>
class /*LIBPBD_API*/ RaRingBuffer
{
protected:

	struct Segment {
#if 0
		Segment ()
			: index (0)
			, write_start_pos (0)
			, write_start_offset (0)
			, write_reversed (false)
		{}
		Segment (Segment const& other)
			: index (other.index)
			, write_start_pos (other.write_start_pos)
			, write_start_offset (other.write_start_offset)
			, write_reversed (other.write_reversed)
		{}
#endif
		guint   index;              // index in ringbuffer of initial start_pos
		int64_t write_start_pos;    // samplepos_t start of write
		int64_t write_start_offset; // sampleoffset_t (data written since _pos); 0: inactive segment
		bool    write_reversed;     // data written to segment is reverse playback
	};

public:
	RaRingBuffer (guint sz, guint res = 8191)
	: reservation (res)
	, _writepos_lock ()
	{
		sz += reservation;

		int32_t power_of_two;
		for (power_of_two = 1; 1U << power_of_two < sz; ++power_of_two);
		size = 1 << power_of_two;

		size_mask = size - 1;
		buf = new T[size];

		read_idx = 0;
		reset (0);
	}

	virtual ~RaRingBuffer () {
		delete [] buf;
	}

	/* non-linear writes need to reset() the buffer and set the
	 * position that write() will commence at */
	void reset (int64_t start) {
		/* writer, when seeking, may block */
		Glib::Threads::Mutex::Lock lm (_reset_lock);

		SpinLock sl (_writepos_lock);

		_seg[0].write_start_pos = start;

		_seg[0].write_start_offset = 0;
		_seg[1].write_start_offset = 0;

		g_atomic_int_set (&write_idx, g_atomic_int_get (&read_idx));
	}

	/* segment public fn
	 *
	 * To keep segments simple POD, these methods are not C++ class members
	 * of struct Segment.
	 */
	bool segment_time_span (Segment const& s, int64_t& frs, int64_t& lrs) const {
		if (s.write_start_offset > 0) {
			lrs = s.write_start_pos + s.write_start_offset;
			frs = lrs - std::min (s.write_start_offset, (int64_t)(size - 1));
			return true;
		}
		lrs = frs = 0;
		return false;
	}

	static inline bool intersect (guint i, guint lower, guint upper) {
		if (lower > upper) {
			return i >= lower || i <= upper;
		} else {
			return i >= lower && i <= upper;
		}
	}

	bool segment_covers_index (Segment const& s, guint idx, guint cnt) const {
		assert (cnt > 0);
		if (s.write_start_offset == 0) {
			return false;
		}

		const guint s0 = s.index;
		const guint s1 = (s0 + (s.write_start_offset - 1)) & size_mask;

		const guint a0 = idx;
		const guint a1 = (idx + (cnt - 1)) & size_mask;

#if 0
		return (s0 > a1) ^ (s1 > a0) ^ (s1 >= a0) ^ (s1 >= s0);
#else
		return intersect (s0, a0, a1)
		    || intersect (s1, a0, a1)
		    || intersect (a0, s0, s1)
		    || intersect (a1, s0, s1);
#endif
	}

	void segment_invalidate (Segment& s, guint idx, guint cnt) const {
		const guint s0 = s.index;
		const guint a1 = (idx + cnt) & size_mask;
		guint delta;
		if  (s0 < a1) {
			delta = a1 - s0;
		} else {
			delta = a1 + (size - s0);
		}
		if (delta > s.write_start_offset) {
			delta = s.write_start_offset;
		}

		printf ("INVAL DELTA %d\n", delta);

		s.index = (s.index + delta) & size_mask;
		s.write_start_pos += delta;
		s.write_start_offset -= delta;
	}

	void invalidate (guint idx, guint cnt) {
		if (segment_covers_index (_seg[0], idx, cnt)) {
			printf ("INVALIDATE SEG 0\n");
			segment_invalidate (_seg[0], idx, cnt);
		}
		if (segment_covers_index (_seg[1], idx, cnt)) {
			printf ("INVALIDATE SEG 1\n");
			segment_invalidate (_seg[1], idx, cnt);
		}
	}


	int segment_to_use (Segment const& s0, Segment const& s1, guint w) const {
		const int d0 = w - s0.index + ((w > s0.index) ? 0 : size);
		const int d1 = w - s1.index + ((w > s1.index) ? 0 : size);
		assert (d0 != d1);
		if (d0 < d1) {
			return 0;
		} else {
			return 1;
		}
	}

	int64_t next_write_pos () const
	{
		Segment s[2];
		guint w;
		{
			SpinLock sl (_writepos_lock);
			s[0] = _seg[0];
			s[1] = _seg[1];
			w = g_atomic_int_get (&write_idx);
		}

		if (s[0].write_start_offset == 0 && s[1].write_start_offset == 0) {
			return s[0].write_start_pos;
		} else if (s[0].write_start_offset != 0) {
			return s[0].write_start_pos + s[0].write_start_offset;
		} else if (s[1].write_start_offset != 0) {
			return s[1].write_start_pos + s[1].write_start_offset;
		} else {
			int segment = segment_to_use (s[0], s[1], w);
			return s[segment].write_start_pos + s[segment].write_start_offset;
		}
	}

	void dump_segments () const {
		Segment s[2];
		{
			SpinLock sl (_writepos_lock);
			s[0] = _seg[0];
			s[1] = _seg[1];
		}
		if (s[0].write_start_offset > 0) {
			int64_t lrs = s[0].write_start_pos + s[0].write_start_offset;
			int64_t frs = lrs - std::min (s[0].write_start_offset, (int64_t)(size - 1));
			printf ("SEGMENT 0:  %ld .. %ld @ %d\n", frs, lrs, s[0].index);
		} else {
			printf ("SEGMENT 0:  --- UNUSED ---\n");
		}

		if (s[1].write_start_offset > 0) {
			int64_t lrs = s[1].write_start_pos + s[1].write_start_offset;
			int64_t frs = lrs - std::min (s[1].write_start_offset, (int64_t)(size - 1));
			printf ("SEGMENT 1:  %ld .. %ld @ %d\n", frs, lrs, s[1].index);
		} else {
			printf ("SEGMENT 1:  --- UNUSED ---\n");
		}
	}

	bool can_read (int64_t start, int32_t cnt) const {
		Segment s[2];
		guint r;
		{
			SpinLock sl (_writepos_lock);
			s[0] = _seg[0];
			s[1] = _seg[1];
			r = g_atomic_int_get (&read_idx);
		}
		// TODO limit.. at most "reservation" earlier than RP
		if (s[0].write_start_offset > 0) {
			int64_t lrs = s[0].write_start_pos + s[0].write_start_offset;
			int64_t frs = lrs - std::min (s[0].write_start_offset, (int64_t)(size - 1));

			if (start >= frs && start + cnt <= lrs) {
#if 0
				int w = (s[0].index + s[0].write_start_offset) & size_mask;

				guint rpos;
				if (w > (lrs - start)) {
					rpos = w - (lrs - start);
				} else {
					rpos = (size + w - (lrs - start)) & size_mask;
				}

				// check if 'r' is in the same segment as rpos.
				// get valid backward boundary of segment where 'r' is
				// ... TODO...
#endif
				return true;
			}
		}

		if (s[1].write_start_offset > 0) {
			int64_t lrs = s[1].write_start_pos + s[1].write_start_offset;
			int64_t frs = lrs - std::min (s[1].write_start_offset, (int64_t)(size - 1));
			if (start >= frs && start + cnt <= lrs) {
				/// TODO check bounds..
				return true;
			}
		}
		printf ("____  READ NOT POSSIBLE %ld .. (+%d) %ld\n", start, cnt, start + cnt);
		dump_segments ();
		printf (" ----\n");

		return false;
	}

#if 0
	void read_range (int64_t &start, int64_t &end) const {
		int64_t frs, lrs;
		{
			SpinLock sl (_writepos_lock);
			lrs = write_start_pos + write_start_offset;
			frs = lrs - std::min (write_start_offset, (int64_t)(size - 1));
		}
		start = frs;
		end = lrs;
	}
#endif

	guint read  (T* dest, int64_t start, guint cnt, bool commit = true);
	guint write (T const* src, int64_t start, guint cnt);

#if 0
	void increment_read_idx (guint cnt) {
		cnt = std::min (cnt, read_space ());
		g_atomic_int_set (&read_idx, (g_atomic_int_get (&read_idx) + cnt) & size_mask);
	}
#endif

	guint write_space () const {
		guint w, r;

		w = g_atomic_int_get (&write_idx);
		r = g_atomic_int_get (&read_idx);

		guint rv;

		if (w > r) {
			rv = (r - w + size) & size_mask;
		} else if (w < r) {
			rv = (r - w);
		} else {
			rv = size;
		}
		return rv - 1;
	}

	guint read_space () const {
		guint w, r;

		w = g_atomic_int_get (&write_idx);
		r = g_atomic_int_get (&read_idx);

		if (w > r) {
			return w - r;
		} else {
			return (w - r + size) & size_mask;
		}
	}

	T *buffer () { return buf; }
	guint bufsize () const { return size; }

protected:
	T *buf;
	guint reservation;
	guint size;
	guint size_mask;

	Segment _seg[2];

	mutable gint write_idx; // corresponds to (write_start_pos + write_start_offset) of active segment
	mutable gint read_idx; // corresponds to most recently read sample_pos

private:
	/* spinlock is used to update  write_start_offset and write_idx in sync */
	mutable spinlock_t   _writepos_lock;
	/* reset_lock is used to prevent concurrent reading and write_start_pos updates. */
	Glib::Threads::Mutex _reset_lock;
};


template<class T> /*LIBPBD_API*/ guint
RaRingBuffer<T>::write (T const *src, int64_t start, guint cnt)
{
	guint free_cnt;
	guint cnt2;
	guint to_write;
	guint n1, n2;
	guint priv_write_idx;
 
	printf ("WRITE %ld .. + %d\n", start, cnt);

	Segment s[2];
	{
		SpinLock sl (_writepos_lock);
		s[0] = _seg[0];
		s[1] = _seg[1];
	}

	int segment = -1;

	priv_write_idx = g_atomic_int_get (&write_idx);

	if (s[0].write_start_offset == 0 && s[1].write_start_offset == 0) {
		// both segments are not used, start write at 1st.
		segment = 0;
		s[0].index = priv_write_idx;
		s[0].write_start_pos = start;
	} else if (s[0].write_start_offset != 0) {
		segment = 0;
	} else if (s[1].write_start_offset != 0) {
		segment = 1;
	} else {
		assert (priv_write_idx == s[0].index || priv_write_idx == s[1].index);
		// both are in use.. find current seg.
		segment = segment_to_use (s[0], s[1], priv_write_idx);
		int64_t next_write = s[segment].write_start_pos + s[segment].write_start_offset;
		if (start != next_write) {
			/* both segments in use */
			printf("RaRingBuffer<>::write -- no free segment to write to\n");
			return 0;
		}
	}

	int64_t next_write = s[segment].write_start_pos + s[segment].write_start_offset;

	int64_t segm_start = next_write - std::min (s[segment].write_start_offset, (int64_t)(size - 1));

	/* find possible overlap */
	guint overlap = 0;
#if 1
	if (start >= segm_start && start < next_write) {
		printf ("  WRITE OVERLAP ------- !!!!!------\n");
		if (start + cnt <= next_write) {
			/* all internal, already written */
			return to_write;
		}
		overlap = next_write - start;
		src += overlap;
		start += overlap;
		cnt -= overlap;
	}
#endif

	if (start != next_write) {
		segment = 1 - segment;
		printf("RaRingBuffer<>::write -- START NEW segment %d\n", segment);
		s[segment].index = priv_write_idx;
		s[segment].write_start_pos = start;
		assert (s[segment].write_start_offset == 0);
		s[segment].write_start_offset = 0;
	}

	{
		SpinLock sl (_writepos_lock);
		if ((free_cnt = write_space ()) == 0) {
			return 0;
		}
		to_write = cnt > free_cnt ? free_cnt : cnt;
		_seg[segment] = s[segment];
		invalidate (priv_write_idx, to_write);
		s[segment] = _seg[segment];
	}

	//assert (priv_write_idx == (s[segment].index + s[segment].write_start_offset) & size_mask);

	cnt2 = priv_write_idx + to_write;

	if (cnt2 > size) {
		n1 = size - priv_write_idx;
		n2 = cnt2 & size_mask;
	} else {
		n1 = to_write;
		n2 = 0;
	}

	memcpy (&buf[priv_write_idx], src, n1 * sizeof (T));
	priv_write_idx = (priv_write_idx + n1) & size_mask;

	if (n2) {
		memcpy (buf, src+n1, n2 * sizeof (T));
		priv_write_idx = n2;
	}

	s[segment].write_start_offset += to_write;
	assert (s[segment].write_start_offset <= size /* - reservation*/);

	{
		SpinLock sl (_writepos_lock);
		_seg[segment] = s[segment];
		g_atomic_int_set (&write_idx, priv_write_idx);
	}
	dump_segments ();
	return to_write + overlap;
}

template<class T> /*LIBPBD_API*/ guint
RaRingBuffer<T>::read (T *dest, int64_t start, guint cnt, bool commit)
{
	Glib::Threads::Mutex::Lock lm (_reset_lock, Glib::Threads::TRY_LOCK);
	if (!lm.locked ()) {
		/* seek, reset in progress */
		return 0;
	}

	guint w, priv_read_idx;
	guint r = g_atomic_int_get (&read_idx);

	Segment s[2];
	{
		SpinLock sl (_writepos_lock);
		s[0] = _seg[0];
		s[1] = _seg[1];
		w = g_atomic_int_get (&write_idx);
	}

	int segment = 0;
	/* see can_read () above */
	if (s[0].write_start_offset > 0) {
		int64_t lrs = s[0].write_start_pos + s[0].write_start_offset;
		int64_t frs = lrs - std::min (s[0].write_start_offset, (int64_t)(size - 1));
		if (start >= frs && start + cnt <= lrs) {
				int p = (s[0].index + s[0].write_start_offset) & size_mask;
				if (p > (lrs - start)) {
					priv_read_idx = p - (lrs - start);
				} else {
					priv_read_idx = (size + p - (lrs - start)) & size_mask;
				}
				segment |= 1;
			// yep.
		}
	}
	if (s[1].write_start_offset > 0) {
		int64_t lrs = s[1].write_start_pos + s[1].write_start_offset;
		int64_t frs = lrs - std::min (s[1].write_start_offset, (int64_t)(size - 1));
		if (start >= frs && start + cnt <= lrs) {
			int p = (s[1].index + s[1].write_start_offset) & size_mask;
			if (p > (lrs - start)) {
				priv_read_idx = p - (lrs - start);
			} else {
				priv_read_idx = (size + p - (lrs - start)) & size_mask;
			}
			segment |= 2;
		}
	}

	if (segment == 3) {
		/* use segment that writer isn't currently using */
		segment = 1 - segment_to_use (s[0], s[1], w);
	}
	else if (segment != 1 && segment != 2) {
		printf ("NOT FOUND %d\n", segment);
		if (commit) {
			g_atomic_int_set (&read_idx, w);
		}
		return 0;
	}

	guint n1, n2;
	guint cnt2 = priv_read_idx + cnt;

	if (cnt2 > size) {
		n1 = size - priv_read_idx;
		n2 = cnt2 & size_mask;
	} else {
		n1 = cnt;
		n2 = 0;
	}

	memcpy (dest, &buf[priv_read_idx], n1 * sizeof (T));
	priv_read_idx = (priv_read_idx + n1) & size_mask;

	if (n2) {
		memcpy (dest + n1, buf, n2 * sizeof (T));
		priv_read_idx = n2;
	}

	if (commit) {
		// move back priv_read_idx by reservation
		// - at most to start of segment
		// - not before write_ptr
		g_atomic_int_set (&read_idx, priv_read_idx);
	}
	return cnt;
}

} /* end namespace */

#endif /* __ringbuffer_h__ */
