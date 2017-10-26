/*
    Copyright (C) 2017 Paul Davis

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifndef __libtemporal_timeline_h__
#define __libtemporal_timeline_h__

#include <ostream>
#include <exception>
#include <string>
#include <cassert>

#include "temporal/types.h"
#include "temporal/beats.h"
#include "temporal/bbt_time.h"
#include "temporal/visibility.h"

namespace Temporal {

class timepos_t;
class TempoMap;

struct TemporalStyleException : public std::exception {
	TemporalStyleException (std::string const & str) : s (str) {}
	const char * what() const throw() { return s.c_str(); }
	std::string s;
};

class LIBTEMPORAL_API PositionLockStatus
{
  public:
	PositionLockStatus () : _style (Temporal::AudioTime), _dirty (Temporal::Dirty (0)) {}
	PositionLockStatus (Temporal::LockStyle ls) : _style (ls), _dirty (Temporal::Dirty (0)) {}
	PositionLockStatus (Temporal::LockStyle ls, Temporal::Dirty d) : _style (ls), _dirty (Temporal::Dirty (d)) {}

	Temporal::LockStyle style() const { return _style; }
	Temporal::Dirty dirty() const { return _dirty; }

	void set_dirty (Temporal::Dirty d) { _dirty = d; }
	void set_style (Temporal::LockStyle s) { _style = s; }
	void clear_dirty () { _dirty = Temporal::Dirty (0); }

  private:
	Temporal::LockStyle _style : 4;
	Temporal::Dirty     _dirty : 4;
};

struct LIBTEMPORAL_API TemporalTypeException : public std::exception {
	TemporalTypeException (std::string const & str) : s (str) {}
	const char * what() const throw() { return s.c_str(); }
	std::string s;
};

class LIBTEMPORAL_API timecnt_t {
  public:
	timecnt_t() : _style (Temporal::AudioTime), _samples (0) {}
	timecnt_t(timepos_t const &);
	timecnt_t(samplepos_t s) : _style (Temporal::AudioTime), _samples (s) {}
	explicit timecnt_t(Temporal::Beats const & b) : _style (Temporal::BeatTime), _beats (b) {}
	explicit timecnt_t(Temporal::BBT_Time const & bbt) : _style (Temporal::BarTime), _bbt (bbt) {}

	static timecnt_t const & max() { return _max_timecnt; }

	Temporal::LockStyle    style()   const { return _style; }

	samplepos_t            samples() const { assert (_style == Temporal::AudioTime); return _samples; }
	Temporal::Beats        beats()   const { assert (_style == Temporal::BeatTime); return _beats; }
	Temporal::BBT_Offset   bbt()     const { assert (_style == Temporal::BarTime); return _bbt; }

	timecnt_t & operator= (samplepos_t s) { _style = Temporal::AudioTime; _samples = s; return *this; }
	timecnt_t & operator= (Temporal::Beats const & b) { _style = Temporal::BeatTime; _beats = b; return *this; }
	timecnt_t & operator= (Temporal::BBT_Offset const & bbt) { _style = Temporal::BarTime; _bbt = bbt; return *this; }
	timecnt_t & operator= (timepos_t const & s);

	timecnt_t operator*(double) const;

	timecnt_t operator-() const {
		switch (_style) {
		case Temporal::AudioTime:
			return timecnt_t (-_samples);
		case Temporal::BeatTime:
			return timecnt_t (-_beats);
		default:
			break;
		}
		throw TemporalTypeException ("cannot perform BBT arithmetic without tempo map");
	}

	timecnt_t operator- (timecnt_t const & t) const {
		assert (_style == t.style());
		switch (_style) {
		case Temporal::AudioTime:
			return timecnt_t (_samples - t._samples);
		case Temporal::BeatTime:
			return timecnt_t (_beats - t._beats);
		default:
			break;
		}
		throw TemporalTypeException ("cannot perform BBT arithmetic without tempo map");
	}

	timecnt_t operator+ (timecnt_t const & t) const {
		assert (_style == t.style());
		switch (_style) {
		case Temporal::AudioTime:
			return timecnt_t (_samples + t._samples);
		case Temporal::BeatTime:
			return timecnt_t (_beats + t._beats);
		default:
			break;
		}
		throw TemporalTypeException ("cannot perform BBT arithmetic without tempo map");
	}

	timecnt_t & operator-= (timecnt_t const & t)  {
		if (this != &t) {
			assert (_style == t.style());
			switch (_style) {
			case Temporal::AudioTime:
				_samples -= t._samples;
			case Temporal::BeatTime:
				_beats -= t._beats;
			case Temporal::BarTime:
				throw TemporalTypeException ("cannot perform BBT arithmetic without tempo map");
			}
		}
		return *this;
	}

	timecnt_t & operator+= (timecnt_t const & t)  {
		if (this != &t) {
			assert (_style == t.style());
			switch (_style) {
			case Temporal::AudioTime:
				_samples += t._samples;
			case Temporal::BeatTime:
				_beats += t._beats;
			case Temporal::BarTime:
				throw TemporalTypeException ("cannot perform BBT arithmetic without tempo map");
			}
		}
		return *this;
	}

	//timecnt_t operator- (timepos_t const & t) const;
	//timecnt_t operator+ (timepos_t const & t) const;
	//timecnt_t & operator-= (timepos_t);
	//timecnt_t & operator+= (timepos_t);

	bool operator> (timecnt_t const & other) const {
		if (_style != other._style) {
			return false;
		}
		switch (_style) {
		case Temporal::AudioTime:
			return _samples > other._samples;
		case Temporal::BeatTime:
			return _beats > other._beats;
		case Temporal::BarTime:
			return _bbt > other._bbt;
		}
		return false;
	}

	bool operator>= (timecnt_t const & other) const {
		if (_style != other._style) {
			return false;
		}
		switch (_style) {
		case Temporal::AudioTime:
			return _samples >= other._samples;
		case Temporal::BeatTime:
			return _beats >= other._beats;
		case Temporal::BarTime:
			return _bbt >= other._bbt;
		}
		return false;
	}

	bool operator< (timecnt_t const & other) const {
		if (_style != other._style) {
			return false;
		}
		switch (_style) {
		case Temporal::AudioTime:
			return _samples < other._samples;
		case Temporal::BeatTime:
			return _beats < other._beats;
		case Temporal::BarTime:
			return _bbt < other._bbt;
		}
		return false;
	}

	bool operator<= (timecnt_t const & other) const {
		if (_style != other._style) {
			return false;
		}
		switch (_style) {
		case Temporal::AudioTime:
			return _samples <= other._samples;
		case Temporal::BeatTime:
			return _beats <= other._beats;
		case Temporal::BarTime:
			return _bbt <= other._bbt;
		}
		return false;
	}

	timecnt_t & operator=(timecnt_t const & other) {
		if (this != &other) {
			_style = other._style;
			switch (_style) {
			case Temporal::AudioTime:
				_samples = other._samples;
			case Temporal::BeatTime:
				_beats = other._beats;
			case Temporal::BarTime:
				_bbt = other._bbt;
			}
		}
		return *this;
	}

	bool operator!= (timecnt_t const & other) const {
		if (_style != other._style) {
			return true;
		}
		switch (_style) {
		case Temporal::AudioTime:
			return _samples != other._samples;
		case Temporal::BeatTime:
			return _beats != other._beats;
		case Temporal::BarTime:
			return _bbt != other._bbt;
		}
		return false;
	}

	bool operator== (timecnt_t const & other) const {
		if (_style != other._style) {
			return false;
		}
		switch (_style) {
		case Temporal::AudioTime:
			return _samples == other._samples;
		case Temporal::BeatTime:
			return _beats == other._beats;
		case Temporal::BarTime:
			return _bbt == other._bbt;
		}
		return false;
	}

	bool string_to (std::string const & str);
	std::string to_string () const;

  private:
	Temporal::LockStyle _style;
	union {
		sampleoffset_t       _samples;
		Temporal::Beats      _beats;
		Temporal::BBT_Offset _bbt;
	};

	static timecnt_t _max_timecnt;
};

class LIBTEMPORAL_API timepos_t {
  public:
	timepos_t ();
	timepos_t (samplepos_t);
	explicit timepos_t (timecnt_t const &); /* will throw() if val is negative */
	explicit timepos_t (Temporal::Beats const &);
	explicit timepos_t (Temporal::BBT_Time const &);

	static timepos_t const & max() { return _max_timepos; }

	samplepos_t            sample() const;
	Temporal::Beats        beats() const;
	Temporal::BBT_Time     bbt() const;

	PositionLockStatus  lock_status() const { return _lock_status; }
	Temporal::LockStyle lock_style() const { return _lock_status.style(); }
	void set_lock_style (Temporal::LockStyle);

	/* these are not thread-safe. in fact, almost nothing in this class is. */
	timepos_t & operator= (timecnt_t const & t); /* will throw() if val is negative */
	timepos_t & operator= (samplepos_t s) { _lock_status.set_style (Temporal::AudioTime);  _lock_status.set_dirty (Temporal::Dirty (Temporal::BeatsDirty|Temporal::BBTDirty)); _samplepos = s; return *this; }
	timepos_t & operator= (Temporal::Beats const & b) { _lock_status.set_style (Temporal::BeatTime);  _lock_status.set_dirty (Temporal::Dirty (Temporal::SampleDirty|Temporal::BBTDirty)); _beats = b; return *this; }
	timepos_t & operator= (Temporal::BBT_Time const & bbt) { _lock_status.set_style (Temporal::BarTime);  _lock_status.set_dirty (Temporal::Dirty (Temporal::BeatsDirty|Temporal::SampleDirty)); _bbt = bbt; return *this; }

	bool operator==(timepos_t const & other) const {
		if (_lock_status.style() != other.lock_status().style()) {
			return false;
		}
		switch (_lock_status.style()) {
		case Temporal::AudioTime:
			return _samplepos == other._samplepos;
		case Temporal::BeatTime:
			return _beats == other._beats;
		case Temporal::BarTime:
			return _bbt == other._bbt;
		}
		return false;
	}

	bool operator!=(timepos_t const & other) const {
		if (_lock_status.style() != other.lock_status().style()) {
			return true;
		}
		switch (_lock_status.style()) {
		case Temporal::AudioTime:
			return _samplepos != other._samplepos;
		case Temporal::BeatTime:
			return _beats != other._beats;
		case Temporal::BarTime:
			return _bbt != other._bbt;
		}
		return false;
	}

	bool operator< (timecnt_t const & other) const {
		switch (other.style()) {
		case Temporal::AudioTime:
			return _samplepos < other.samples();
		case Temporal::BeatTime:
			return _beats < other.beats ();
		case Temporal::BarTime:
			return _bbt < other.bbt();
		}
		return false;
	}

	bool operator> (timecnt_t const & other) const {
		switch (other.style()) {
		case Temporal::AudioTime:
			return _samplepos > other.samples();
		case Temporal::BeatTime:
			return _beats > other.beats ();
		case Temporal::BarTime:
			return _bbt > other.bbt();
		}
		return false;
	}

	bool operator<= (timecnt_t const & other) const {
		switch (other.style()) {
		case Temporal::AudioTime:
			return _samplepos <= other.samples();
		case Temporal::BeatTime:
			return _beats <= other.beats ();
		case Temporal::BarTime:
			return _bbt <= other.bbt();
		}
		return false;
	}

	bool operator>= (timecnt_t const & other) const {
		switch (other.style()) {
		case Temporal::AudioTime:
			return _samplepos >= other.samples();
		case Temporal::BeatTime:
			return _beats >= other.beats ();
		case Temporal::BarTime:
			return _bbt >= other.bbt();
		}
		return false;
	}

	bool operator< (timepos_t const & other) const {
		switch (_lock_status.style()) {
		case Temporal::AudioTime:
			return _samplepos < other.sample();
		case Temporal::BeatTime:
			return _beats < other.beats ();
		case Temporal::BarTime:
			return _bbt < other.bbt();
		}
		return false;
	}

	bool operator> (timepos_t const & other) const {
		switch (_lock_status.style()) {
		case Temporal::AudioTime:
			return _samplepos > other.sample();
		case Temporal::BeatTime:
			return _beats > other.beats ();
		case Temporal::BarTime:
			return _bbt > other.bbt();
		}
		return false;
	}

	bool operator<= (timepos_t const & other) const {
		switch (_lock_status.style()) {
		case Temporal::AudioTime:
			return _samplepos <= other.sample();
		case Temporal::BeatTime:
			return _beats <= other.beats ();
		case Temporal::BarTime:
			return _bbt <= other.bbt();
		}
		return false;
	}

	timepos_t operator+(timecnt_t d) const;
	timepos_t operator+(timepos_t d) const;
	timepos_t operator+(samplepos_t) const;
	timepos_t operator+(Temporal::Beats const &) const;
	timepos_t operator+(Temporal::BBT_Offset const &) const;

	timepos_t operator-(timecnt_t d) const;
	timepos_t operator-(timepos_t d) const;
	timepos_t operator-(samplepos_t) const;
	timepos_t operator-(Temporal::Beats const &) const;
	timepos_t operator-(Temporal::BBT_Offset const &) const;

	timepos_t operator*(double) const;
	timepos_t operator*(int) const;
	timepos_t & operator*=(double);
	timepos_t & operator*=(int);

	timepos_t & operator+=(timecnt_t d);
	timepos_t & operator+=(samplepos_t);
	timepos_t & operator+=(Temporal::Beats const &);
	timepos_t & operator+=(Temporal::BBT_Offset const &);

	timepos_t & operator-=(timecnt_t d);
	timepos_t & operator-=(samplepos_t);
	timepos_t & operator-=(Temporal::Beats const &);
	timepos_t & operator-=(Temporal::BBT_Offset const &);

	void set_sample (samplepos_t s);
	void set_beat (Temporal::Beats const &);
	void set_bbt (Temporal::BBT_Time const &);

	bool string_to (std::string const & str);
	std::string to_string () const;

	static void set_tempo_map (TempoMap& tm) { _tempo_map = &tm; }

  private:
	mutable int         update_generation;
	PositionLockStatus _lock_status;

	/* these are mutable because we may need to update them at arbitrary
	   times, even within contexts that are otherwise const. For example, an
	   audio-locked position whose _beats value is out of date. The audio time
	   is canonical and will not change, but beats() needs to be callable, and
	   we'd prefer to claim const-ness for it than not.
	*/

	mutable samplepos_t        _samplepos;
	mutable Temporal::Beats    _beats;
	mutable Temporal::BBT_Time _bbt;

	static TempoMap* _tempo_map;

	void update_music_time ();
	void update_audio_time ();

	/* special constructor for max_timepos */
	timepos_t (PositionLockStatus);
	static timepos_t _max_timepos;

	/* these do not affect the canonical position value, and so we label
	   them const for ease of use in const contexts.
	*/
	void update_audio_and_bbt_times () const;
	void update_audio_and_beat_times () const;
	void update_music_times () const;
};

}

namespace std {
std::ostream & operator<< (std::ostream &, Temporal::timepos_t const &);
std::ostream & operator<< (std::ostream &, Temporal::timecnt_t const &);
}
inline static bool operator< (Temporal::samplepos_t s, Temporal::timepos_t const & t) { return s < t.sample(); }
inline static bool operator< (Temporal::Beats const & b, Temporal::timepos_t const & t) { return b < t.beats(); }
inline static bool operator< (Temporal::BBT_Time const & bbt, Temporal::timepos_t const & t) { return bbt < t.bbt(); }

inline static bool operator> (Temporal::samplepos_t s, Temporal::timepos_t const & t) { return s > t.sample(); }
inline static bool operator> (Temporal::Beats const & b, Temporal::timepos_t const & t) { return b > t.beats(); }
inline static bool operator> (Temporal::BBT_Time const & bbt, Temporal::timepos_t const & t) { return bbt > t.bbt(); }

inline static bool operator< (Temporal::timepos_t const & t, Temporal::samplepos_t s) { return t.sample() < s; }
inline static bool operator< (Temporal::timepos_t const & t, Temporal::Beats const & b) { return t.beats() < b; }
inline static bool operator< (Temporal::timepos_t const & t, Temporal::BBT_Time const & bbt) { return t.bbt() < bbt; }

inline static bool operator> (Temporal::timepos_t const & t, Temporal::samplepos_t s) { return t.sample() > s; }
inline static bool operator> (Temporal::timepos_t const & t, Temporal::Beats const & b) { return t.beats() > b; }
inline static bool operator> (Temporal::timepos_t const & t, Temporal::BBT_Time const & bbt) { return t.bbt() > bbt; }

inline static bool operator< (Temporal::samplepos_t s, Temporal::timecnt_t const & t) { assert (t.style() == Temporal::AudioTime); return s < t.samples(); }
inline static bool operator< (Temporal::Beats const & b, Temporal::timecnt_t const & t) { assert (t.style() == Temporal::BeatTime); return b < t.beats(); }
inline static bool operator< (Temporal::BBT_Time const & bbt, Temporal::timecnt_t const & t) { assert (t.style() == Temporal::BarTime); return bbt < t.bbt(); }

inline static bool operator> (Temporal::samplepos_t s, Temporal::timecnt_t const & t) { assert (t.style() == Temporal::AudioTime); return s > t.samples(); }
inline static bool operator> (Temporal::Beats const & b, Temporal::timecnt_t const & t) { assert (t.style() == Temporal::BeatTime); return b > t.beats(); }
inline static bool operator> (Temporal::BBT_Time const & bbt, Temporal::timecnt_t const & t) { assert (t.style() == Temporal::BarTime); return bbt > t.bbt(); }

inline static bool operator< (Temporal::timecnt_t const & t, Temporal::samplepos_t s) { assert (t.style() == Temporal::AudioTime); return t.samples() < s; }
inline static bool operator< (Temporal::timecnt_t const & t, Temporal::Beats const & b) { assert (t.style() == Temporal::BeatTime); return t.beats() < b; }
inline static bool operator< (Temporal::timecnt_t const & t, Temporal::BBT_Time const & bbt) { assert (t.style() == Temporal::BarTime); return t.bbt() < bbt; }

inline static bool operator> (Temporal::timecnt_t const & t, Temporal::samplepos_t s) { assert (t.style() == Temporal::AudioTime); return t.samples() > s; }
inline static bool operator> (Temporal::timecnt_t const & t, Temporal::Beats const & b) { assert (t.style() == Temporal::BeatTime); return t.beats() > b; }
inline static bool operator> (Temporal::timecnt_t const & t, Temporal::BBT_Time const & bbt) { assert (t.style() == Temporal::BarTime); return t.bbt() > bbt; }


#endif /* __libtemporal_timeline_h__ */
