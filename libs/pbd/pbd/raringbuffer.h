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

template<class T>
class /*LIBPBD_API*/ RaRingBuffer
{
public:
	RaRingBuffer (int32_t sz, guint res = 8191)
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
		write_start_pos = start;
		write_start_offset = 0;
		g_atomic_int_set (&write_idx, g_atomic_int_get (&read_idx));
	}

	int64_t next_write_pos () const
	{
		int64_t lrs;
		{
			SpinLock sl (_writepos_lock);
			lrs = write_start_pos + write_start_offset;
		}
		return lrs;
	}

	bool can_read (int64_t start, int32_t cnt) const {
		int64_t frs, lrs;
		{
			SpinLock sl (_writepos_lock);
			lrs = write_start_pos + write_start_offset;
			frs = lrs - std::min (write_start_offset, (int64_t)(size - 1));
		}
		if (start >= frs &&  start + cnt < lrs) {
			return true;
		}
		return false;
	}

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

	guint read  (T* dest, int64_t start, guint cnt);
	guint write (T const* src, guint cnt);

	void increment_read_idx (guint cnt) {
		cnt = std::min (cnt, read_space ());
		g_atomic_int_set (&read_idx, (g_atomic_int_get (&read_idx) + cnt) & size_mask);
	}

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
		/* it may hapen that the read/invalidation-pointer moves backwards
		 * e.g. after rec-stop, declick fade-out.
		 * At the same time the butler may already have written data.
		 * (it's safe as long as the disk-reader does not move backwards by more
		 * than reservation)
		 * XXX disk-reading de-click should not move the invalidation-pointer
		 */
		//assert (rv > reservation);
		if (rv > reservation) {
			return rv - 1 - reservation;
		}
		return 0;
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
	guint get_write_idx () const { return g_atomic_int_get (&write_idx); }
	guint get_read_idx () const { return g_atomic_int_get (&read_idx); }
	guint bufsize () const { return size; }

	void set_read_pos (int64_t pos) {/* TODO, set read_idx to sample corresponding to pos */ }
	void read_flush () { g_atomic_int_set (&read_idx, g_atomic_int_get (&write_idx)); }

protected:
	T *buf;
	guint reservation;
	guint size;
	guint size_mask;

	int64_t write_start_pos; // samplepos_t
	int64_t write_start_offset; // sampleoffset_t

	mutable gint write_idx; // corresponds to (write_start_pos + write_start_offset)
	mutable gint read_idx; // corresponds to most recently read sample_pos

private:
	/* spinlock is used to update  write_start_offset and write_idx in sync */
	mutable spinlock_t   _writepos_lock;
	/* reset_lock is used to prevent concurrent reading and write_start_pos updates. */
	Glib::Threads::Mutex _reset_lock;
};


template<class T> /*LIBPBD_API*/ guint
RaRingBuffer<T>::write (T const *src, guint cnt)
{
	guint free_cnt;
	guint cnt2;
	guint to_write;
	guint n1, n2;
	guint priv_write_idx;

	priv_write_idx = g_atomic_int_get (&write_idx);

	if ((free_cnt = write_space ()) == 0) {
		return 0;
	}

	to_write = cnt > free_cnt ? free_cnt : cnt;

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

	{
		SpinLock sl (_writepos_lock);
		write_start_offset += to_write;
		g_atomic_int_set (&write_idx, priv_write_idx);
	}
	return to_write;
}

template<class T> /*LIBPBD_API*/ guint
RaRingBuffer<T>::read (T *dest, int64_t start, guint cnt)
{
	int64_t frs, lrs; // first and last readable sample
	guint w, r;

	Glib::Threads::Mutex::Lock lm (_reset_lock, Glib::Threads::TRY_LOCK);
	if (!lm.locked ()) {
		/* seek, reset in progress */
		return 0;
	}

	r = g_atomic_int_get (&read_idx);
	{
		SpinLock sl (_writepos_lock);
		w = g_atomic_int_get (&write_idx);
		lrs = write_start_pos + write_start_offset;
		frs = lrs - std::min (write_start_offset, (int64_t)(size - 1));
	}

	if (start > lrs) {
		/* data written to buffer is too old.
		 * clear up space in buffer for writer.
		 */
		g_atomic_int_set (&read_idx, w);
		return 0;
	}

	if (start < frs || start + cnt >= lrs) {
		/* required range not present */
		return 0;
	}

	/* write_idx corresponds to lrs,
	 * calculate read-index corresponds to start
	 */
	guint priv_read_idx;
	assert (abs (w - (lrs - start)) < size);
	if ((lrs - start) < w) {
		priv_read_idx = w - (lrs - start);
	} else {
		priv_read_idx = (size + w - (lrs - start)) & size_mask;
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

	/* set read-pointer to position of last read's end */
	g_atomic_int_set (&read_idx, priv_read_idx);
	return cnt;
}

} /* end namespace */

#endif /* __ringbuffer_h__ */
