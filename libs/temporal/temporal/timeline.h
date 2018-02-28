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
#include <limits>

#include "pbd/enumwriter.h"

#include "temporal/types.h"
#include "temporal/beats.h"
#include "temporal/bbt_time.h"
#include "temporal/visibility.h"

namespace Temporal {

class timecnt_t;
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
	PositionLockStatus (Temporal::TimeDomain ls) : _style (ls), _dirty (Temporal::Dirty (0)) {}
	PositionLockStatus (Temporal::TimeDomain ls, Temporal::Dirty d) : _style (ls), _dirty (Temporal::Dirty (d)) {}

	Temporal::TimeDomain style() const { return _style; }
	Temporal::Dirty dirty() const { return _dirty; }

	void set_dirty (Temporal::Dirty d) { _dirty = d; }
	void set_style (Temporal::TimeDomain s) { _style = s; }
	void clear_dirty () { _dirty = Temporal::Dirty (0); }

  private:
	Temporal::TimeDomain _style : 4;
	Temporal::Dirty     _dirty : 4;
};

struct LIBTEMPORAL_API TemporalTypeException : public std::exception {
	TemporalTypeException (std::string const & str) : s (str) {}
	const char * what() const throw() { return s.c_str(); }
	std::string s;
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
	Temporal::TimeDomain lock_style() const { return _lock_status.style(); }
	void set_lock_style (Temporal::TimeDomain);

	/* return a timepos_t that is the next (later) possible position given
	 * this one
	 */
	timepos_t increment () const {
		switch (_lock_status.style()) {
		case Temporal::BeatTime:
			return timepos_t (_beats + Beats (1));
		case Temporal::AudioTime:
			return timepos_t (_samplepos < max_samplepos ? _samplepos + 1 : _samplepos);
		default:
			/* can't do math on BBT time, see bbt_time.h for details */
			break;
		}

		abort ();
	}

	/* return a timepos_t that is the previous (earlier) possible position given
	 * this one
	 */
	timepos_t decrement () const {
		switch (_lock_status.style()) {
		case Temporal::BeatTime:
			return timepos_t (_beats - Beats (1)); /* beats can go negative */
		case Temporal::AudioTime:
			return timepos_t (_samplepos > 0 ? _samplepos - 1 : _samplepos); /* samples cannot go negative */
		default:
			/* can't do math on BBT time, see bbt_time.h for details */
			break;
		}

		abort ();
	}

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

	bool operator<  (timecnt_t const & other) const;
	bool operator>  (timecnt_t const & other) const;
	bool operator<= (timecnt_t const & other) const;
	bool operator>= (timecnt_t const & other) const;

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

	bool operator>= (timepos_t const & other) const {
		switch (_lock_status.style()) {
		case Temporal::AudioTime:
			return _samplepos >= other.sample();
		case Temporal::BeatTime:
			return _beats >= other.beats ();
		case Temporal::BarTime:
			return _bbt >= other.bbt();
		}
		return false;
	}

	timepos_t operator+(timecnt_t const & d) const;
	timepos_t operator+(timepos_t const & d) const;
	timepos_t operator+(samplepos_t) const;
	timepos_t operator+(Temporal::Beats const &) const;
	timepos_t operator+(Temporal::BBT_Offset const &) const;

	/* operator-() poses severe and thorny problems for a class that represents position on a timeline.

	   If the value of the class is a simple scalar, then subtraction can be used for both:

	     1) movement backwards along the timeline
	     2) computing the distance between two positions

	   But timepos_t is not a simple scalar, and neither is timecnt_t, and these two operations are quite different.

	     1) movement backwards along the timeline should result in another timepos_t
             2) the distance between two positions is a timecnt_t

           so already we have a hint that we would need at least:

              timepos_t operator- (timecnt_t const &); ... compute new position
              timecnt_t operator- (timepos_t const &); ... compute distance

            But what happens we try to use more explicit types. What does this expression mean:

              timepos_t pos;
              pos - Beats (3);

            is this computing a new position 3 beats earlier than pos? or is it computing the distance between
            pos and the 3rd beat?

            For this reason, we do not provide any operator-() methods, but instead require the use of
            explicit methods with clear semantics.
	*/

	/* delta generates a timecnt_t, which may be negative

	   inclusive delta:
	   if the result is termed r, then this + r = d

	   exclusive delta:
	   if the result is termed r, then this + r = last position before d
	*/

	/* computes the distance between this timepos_t and @param p
	   such that: this + distance = p

	   This means that if @param p is later than this, distance is positive;
	   if @param p is earlier than this, distance is negative.

	   Note that the return value is a timecnt_t whose position member
	   is equal to the value of this. That means if the distance uses
	   musical time value, the distance may not have constant value
	   at other positions on the timeline.
	*/

	timecnt_t distance (timecnt_t const & p) const;
	timecnt_t distance (timepos_t const & p) const;
	timecnt_t distance (samplepos_t) const;
	timecnt_t distance (Temporal::Beats const &) const;
	timecnt_t distance (Temporal::BBT_Offset const &) const;

	/* this is a godawful hack to cover some cases where an endpoint is inclusive.
	   Plan is to remove this, once Range uses an exclusive end.
	*/

	timecnt_t inclusive_delta (timecnt_t const & d) const;
	timecnt_t inclusive_delta (timepos_t const & d) const;
	timecnt_t inclusive_delta (samplepos_t d) const;
	timecnt_t inclusive_delta (Temporal::Beats const & d) const;
	timecnt_t inclusive_delta (Temporal::BBT_Offset const & d) const;

	/* computes a new position value that is @param d earlier than this */

	timepos_t earlier (timepos_t const & d) const; /* treat d as distance measured from timeline origin */
	timepos_t earlier (timecnt_t const & d) const;
	timepos_t earlier (samplepos_t d) const;
	timepos_t earlier (Beats const & d) const;
	timepos_t earlier (BBT_Offset const & d) const;

	/* like ::earlier() but changes this. loosely equivalent to operator-= */

	timepos_t & shift_earlier (timecnt_t const & d);
	timepos_t & shift_earlier (samplepos_t);
	timepos_t & shift_earlier (Temporal::Beats const &);
	timepos_t & shift_earlier (Temporal::BBT_Offset const &);

	timepos_t operator/(double) const;
	timepos_t operator*(double) const;

	timepos_t & operator*=(double);
	timepos_t & operator/=(double);

	timepos_t & operator+=(timecnt_t const & d);
	timepos_t & operator+=(samplepos_t);
	timepos_t & operator+=(Temporal::Beats const &);
	timepos_t & operator+=(Temporal::BBT_Offset const &);

	timepos_t   operator% (timecnt_t const &) const;
	timepos_t & operator%=(timecnt_t const &);

	bool operator<  (Temporal::samplepos_t s) { return sample() < s; }
	bool operator<  (Temporal::Beats const & b) { return beats() < b; }
	bool operator<  (Temporal::BBT_Time const & bb) { return bbt() < bb; }
	bool operator<= (Temporal::samplepos_t s) { return sample() <= s; }
	bool operator<= (Temporal::Beats const & b) { return beats() <= b; }
	bool operator<= (Temporal::BBT_Time const & bb) { return bbt() <= bb; }
	bool operator>  (Temporal::samplepos_t s) { return sample() > s; }
	bool operator>  (Temporal::Beats const & b) { return beats() > b; }
	bool operator>  (Temporal::BBT_Time const & bb) { return bbt() > bb; }
	bool operator>= (Temporal::samplepos_t s) { return sample() >= s; }
	bool operator>= (Temporal::Beats const & b) { return beats() >= b; }
	bool operator>= (Temporal::BBT_Time const & bb) { return bbt() >= bb; }
	bool operator== (Temporal::samplepos_t s) { return sample() == s; }
	bool operator== (Temporal::Beats const & b) { return beats() == b; }
	bool operator== (Temporal::BBT_Time const & bb) { return bbt() == bb; }
	bool operator!= (Temporal::samplepos_t s) { return sample() != s; }
	bool operator!= (Temporal::Beats const & b) { return beats() != b; }
	bool operator!= (Temporal::BBT_Time const & bb) { return bbt() != bb; }

	void set_sample (samplepos_t s);
	void set_beat (Temporal::Beats const &);
	void set_bbt (Temporal::BBT_Time const &);

	bool string_to (std::string const & str);
	std::string to_string () const;

	/* these do not affect the canonical position value, and so we label
	   them const for ease of use in const contexts.
	*/
	void update_audio_and_bbt_times () const;
	void update_audio_and_beat_times () const;
	void update_music_times () const;

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

};

/**
 * a timecnt_t measures a duration in a specified time domain and starting at a
 * specific position.
 *
 * It can be freely converted between time domains, as well as used as the
 * subject of most arithmetic operations.
 *
 * An important distinction between timepos_t and timecnt_t can be thought of
 * this way: a timepos_t ALWAYS refers to a position relative to the origin of
 * the timeline (technically, the origin in the tempo map used to translate
 * between audio and musical domains). By contrast, a timecnt_t refers to a
 * certain distance beyond some arbitrary (specified) origin. So, a timepos_t
 * of "3 beats" always means "3 beats measured from the timeline origin". A
 * timecnt_t of "3 beats" always come with a position, and so is really "3
 * beats after <position>".
 *
 * The ambiguity surrounding operator-() that affects timepos_t does not exist
 * for timecnt_t: all uses of operator-() are intended to compute the result of
 * subtracting one timecnt_t from another which will always result in another
 * timecnt_t of lesser value than the first operand.
 */

class LIBTEMPORAL_API timecnt_t {
  public:
	timecnt_t () : _style (Temporal::AudioTime), _samples (0) {}
	timecnt_t (timepos_t const &, timepos_t const & pos);
	timecnt_t (timecnt_t const &, timepos_t const & pos);
	timecnt_t (samplepos_t s, timepos_t const & pos) : _style (Temporal::AudioTime), _samples (s), _position (pos) {}
	explicit timecnt_t (Temporal::Beats const & b, timepos_t const & pos) : _style (Temporal::BeatTime), _beats (b), _position (pos) {}
	explicit timecnt_t (Temporal::BBT_Offset const & bbt, timepos_t const & pos) : _style (Temporal::BarTime), _bbt (bbt), _position (pos) {}

	/* provides a more compact form and faster execution for "timecnt_t > 0 */
	bool positive() const {
		switch (_style) {
		case BarTime: return _bbt > BBT_Offset (0, 0, 0);
		case BeatTime: return _beats > Beats (0, 0);
		case AudioTime: break;
		}
		return _samples > 0;
	}

	/* provides a more compact form and faster execution for "timecnt_t < 0 */
	bool negative() const {
		switch (_style) {
		case BarTime: return _bbt < BBT_Offset (0, 0, 0);
		case BeatTime: return _beats < Beats (0, 0);
		case AudioTime: break;
		}
		return _samples < 0;
	}

	/* provides a more compact form and faster execution for "timecnt_t == 0 */
	bool zero() const {
		switch (_style) {
		case BarTime: return _bbt == BBT_Offset();
		case BeatTime: return _beats == Beats (0, 0);
		case AudioTime: break;
		}
		return _samples == 0;
	}

	void set_position (timepos_t const &pos);
	timepos_t const & position() const { return _position; }

	static timecnt_t const & max() { return _max_timecnt; }

	timecnt_t abs() const;

	Temporal::TimeDomain    style()   const { return _style; }

	samplepos_t            samples() const { switch (_style) { case Temporal::AudioTime: return _samples; default: break; } return compute_samples (); }
	Temporal::Beats        beats  () const { switch (_style) { case Temporal::BeatTime: return _beats; default: break; } return compute_beats (); }
	Temporal::BBT_Offset   bbt    () const { switch (_style) { case Temporal::BarTime: return _bbt; default: break; } return compute_bbt (); }

	timecnt_t & operator= (samplepos_t s) { _style = Temporal::AudioTime; _samples = s; return *this; }
	timecnt_t & operator= (Temporal::Beats const & b) { _style = Temporal::BeatTime; _beats = b; return *this; }
	timecnt_t & operator= (Temporal::BBT_Offset const & bbt) { _style = Temporal::BarTime; _bbt = bbt; return *this; }
	timecnt_t & operator= (timepos_t const & s);

	/* return a timecnt_t that is the next (later) possible position given
	 * this one
	 */
	timecnt_t increment () const {
		switch (_style) {
		case Temporal::BeatTime:
			return timecnt_t (_beats + Beats (0, 1), _position);
		case Temporal::AudioTime:
			return timecnt_t (_samples < max_samplepos ? _samples + 1 : _samples, _position);
		default:
			/* can't do math on BBT time, see bbt_time.h for details */
			break;
		}

		abort ();
	}

	timecnt_t decrement () const {
		switch (_style) {
		case Temporal::BeatTime:
			return timecnt_t (_beats - Beats (0, 1), _position); /* beats can go negative */
		case Temporal::AudioTime:
			return timecnt_t (_samples > 0 ? _samples - 1 : _samples, _position); /* samples cannot go negative */
		default:
			/* can't do math on BBT time, see bbt_time.h for details */
			break;
		}

		abort ();
	}

	timecnt_t operator*(double) const;
	timecnt_t operator/(double n) const;

	timecnt_t operator-() const {
		switch (_style) {
		case Temporal::AudioTime:
			return timecnt_t (-_samples, _position);
		case Temporal::BeatTime:
			return timecnt_t (-_beats, _position);
		default:
			break;
		}
		throw TemporalTypeException ("cannot perform BBT arithmetic without tempo map");
	}

	timecnt_t operator- (timecnt_t const & t) const {
		switch (_style) {
		case Temporal::AudioTime:
			return timecnt_t (_samples - t.samples(), _position);
		case Temporal::BeatTime:
			return timecnt_t (_beats - t.beats(), _position);
		default:
			break;
		}
		throw TemporalTypeException ("cannot perform BBT arithmetic without tempo map");
	}

	timecnt_t operator+ (timecnt_t const & t) const {
		switch (_style) {
		case Temporal::AudioTime:
			return timecnt_t (_samples + t.samples(), _position);
		case Temporal::BeatTime:
			return timecnt_t (_beats + t.beats(), _position);
		default:
			break;
		}
		throw TemporalTypeException ("cannot perform BBT arithmetic without tempo map");
	}

	timecnt_t & operator-= (timecnt_t const & t)  {
		if (this != &t) {
			switch (_style) {
			case Temporal::AudioTime:
				_samples -= t.samples();
			case Temporal::BeatTime:
				_beats -= t.beats();
			case Temporal::BarTime:
				throw TemporalTypeException ("cannot perform BBT arithmetic without tempo map");
			}
		}
		return *this;
	}

	timecnt_t & operator+= (timecnt_t const & t)  {
		if (this != &t) {
			switch (_style) {
			case Temporal::AudioTime:
				_samples += t.samples();
			case Temporal::BeatTime:
				_beats += t.beats();
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
		switch (_style) {
		case Temporal::AudioTime:
			return _samples > other.samples();
		case Temporal::BeatTime:
			return _beats > other.beats();
		case Temporal::BarTime:
			return _bbt > other.bbt();
		}
		return false;
	}

	bool operator>= (timecnt_t const & other) const {
		switch (_style) {
		case Temporal::AudioTime:
			return _samples >= other.samples();
		case Temporal::BeatTime:
			return _beats >= other.beats();
		case Temporal::BarTime:
			return _bbt >= other.bbt();
		}
		return false;
	}

	bool operator< (timecnt_t const & other) const {
		switch (_style) {
		case Temporal::AudioTime:
			return _samples < other.samples();
		case Temporal::BeatTime:
			return _beats < other.beats();
		case Temporal::BarTime:
			return _bbt < other.bbt();
		}
		return false;
	}

	bool operator<= (timecnt_t const & other) const {
		switch (_style) {
		case Temporal::AudioTime:
			return _samples <= other.samples();
		case Temporal::BeatTime:
			return _beats <= other.beats();
		case Temporal::BarTime:
			return _bbt <= other.bbt();
		}
		return false;
	}

	timecnt_t & operator=(timecnt_t const & other) {
		if (this != &other) {
			_style = other._style;
			switch (other._style) {
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
		switch (_style) {
		case Temporal::AudioTime:
			return _samples != other.samples();
		case Temporal::BeatTime:
			return _beats != other.beats();
		case Temporal::BarTime:
			return _bbt != other.bbt();
		}
		return false;
	}

	bool operator== (timecnt_t const & other) const {
		switch (_style) {
		case Temporal::AudioTime:
			return _samples == other.samples();
		case Temporal::BeatTime:
			return _beats == other.beats();
		case Temporal::BarTime:
			return _bbt == other.bbt();
		}
		return false;
	}

	/* test for numerical equivalence with a timepos_T. This tests ONLY the
	   duration in the given domain, NOT position.
	*/
	bool operator== (timepos_t const & other) const {
		switch (_style) {
		case Temporal::AudioTime:
			return _samples == other.sample();
		case Temporal::BeatTime:
			return _beats == other.beats();
		case Temporal::BarTime:
			return _bbt == other.bbt();
		}
		return false;
	}

	bool operator< (Temporal::samplepos_t s) { return samples() < s; }
	bool operator< (Temporal::Beats const & b) { return beats() < b; }
	bool operator< (Temporal::BBT_Time const & bb) { return bbt() < bb; }
	bool operator<= (Temporal::samplepos_t s) { return samples() <= s; }
	bool operator<= (Temporal::Beats const & b) { return beats() <= b; }
	bool operator<= (Temporal::BBT_Time const & bb) { return bbt() <= bb; }
	bool operator> (Temporal::samplepos_t s) { return samples() > s; }
	bool operator> (Temporal::Beats const & b) { return beats() > b; }
	bool operator> (Temporal::BBT_Time const & bb) { return bbt() > bb; }
	bool operator>= (Temporal::samplepos_t s) { return samples() >= s; }
	bool operator>= (Temporal::Beats const & b) { return beats() >= b; }
	bool operator>= (Temporal::BBT_Time const & bb) { return bbt() >= bb; }
	bool operator== (Temporal::samplepos_t s) { return samples() == s; }
	bool operator== (Temporal::Beats const & b) { return beats() == b; }
	bool operator== (Temporal::BBT_Time const & bb) { return bbt() == bb; }
	bool operator!= (Temporal::samplepos_t s) { return samples() != s; }
	bool operator!= (Temporal::Beats const & b) { return beats() != b; }
	bool operator!= (Temporal::BBT_Time const & bb) { return bbt() != bb; }

	timecnt_t   operator% (timecnt_t const &) const;
	timecnt_t & operator%=(timecnt_t const &);

	bool string_to (std::string const & str);
	std::string to_string () const;

	static void set_tempo_map (TempoMap& tm) { _tempo_map = &tm; }

  private:
	Temporal::TimeDomain _style;
	union {
		sampleoffset_t       _samples;
		Temporal::Beats      _beats;
		Temporal::BBT_Offset _bbt;
	};

	timepos_t _position;

	static timecnt_t _max_timecnt;
	static TempoMap* _tempo_map;

	samplepos_t compute_samples () const;
	Beats compute_beats () const;
	BBT_Offset compute_bbt () const;
};

inline timecnt_t
timepos_t::inclusive_delta (timecnt_t const & d) const
{
	return distance (d).increment();
}

inline timecnt_t
timepos_t::inclusive_delta (timepos_t const & d) const
{
	return distance (d).increment();
}

inline timecnt_t
timepos_t::inclusive_delta (samplepos_t d) const
{
	return distance (d).increment();
}

inline timecnt_t
timepos_t::inclusive_delta (Temporal::Beats const & d) const
{
	return distance (d).increment();
}

inline timecnt_t
timepos_t::inclusive_delta (Temporal::BBT_Offset const & d) const
{
	return distance (d).increment();
}

inline bool
timepos_t::operator< (timecnt_t const & other) const
{
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

inline bool
timepos_t::operator> (timecnt_t const & other) const
{
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

inline bool
timepos_t::operator<= (timecnt_t const & other) const
{
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

inline bool
timepos_t::operator>= (timecnt_t const & other) const
{
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

} /* end of namespace Temporal */

namespace std {
std::ostream & operator<< (std::ostream &, Temporal::timepos_t const &);
std::ostream & operator<< (std::ostream &, Temporal::timecnt_t const &);
}
inline static bool operator< (Temporal::samplepos_t s, Temporal::timepos_t const & t) { return s < t.sample(); }
inline static bool operator< (Temporal::Beats const & b, Temporal::timepos_t const & t) { return b < t.beats(); }
inline static bool operator< (Temporal::BBT_Time const & bbt, Temporal::timepos_t const & t) { return bbt < t.bbt(); }

inline static bool operator<= (Temporal::samplepos_t s, Temporal::timepos_t const & t) { return s <= t.sample(); }
inline static bool operator<= (Temporal::Beats const & b, Temporal::timepos_t const & t) { return b <= t.beats(); }
inline static bool operator<= (Temporal::BBT_Time const & bbt, Temporal::timepos_t const & t) { return bbt <= t.bbt(); }

inline static bool operator> (Temporal::samplepos_t s, Temporal::timepos_t const & t) { return s > t.sample(); }
inline static bool operator> (Temporal::Beats const & b, Temporal::timepos_t const & t) { return b > t.beats(); }
inline static bool operator> (Temporal::BBT_Time const & bbt, Temporal::timepos_t const & t) { return bbt > t.bbt(); }

inline static bool operator>= (Temporal::samplepos_t s, Temporal::timepos_t const & t) { return s >= t.sample(); }
inline static bool operator>= (Temporal::Beats const & b, Temporal::timepos_t const & t) { return b >= t.beats(); }
inline static bool operator>= (Temporal::BBT_Time const & bbt, Temporal::timepos_t const & t) { return bbt >= t.bbt(); }

#undef TEMPORAL_DOMAIN_WARNING
#define TEMPORAL_DOMAIN_WARNING(d) if (t.style() != (d)) std::cerr << "DOMAIN CONVERSION WARNING IN COMPARATOR with t.domain = " << enum_2_string (t.style()) << " not " << enum_2_string (d) << std::endl;

inline static bool operator< (Temporal::samplepos_t s, Temporal::timecnt_t const & t) { TEMPORAL_DOMAIN_WARNING (Temporal::AudioTime); return s < t.samples(); }
inline static bool operator< (Temporal::Beats const & b, Temporal::timecnt_t const & t) { TEMPORAL_DOMAIN_WARNING (Temporal::BeatTime); return b < t.beats(); }
inline static bool operator< (Temporal::BBT_Time const & bbt, Temporal::timecnt_t const & t) { TEMPORAL_DOMAIN_WARNING (Temporal::BarTime); return bbt < t.bbt(); }

inline static bool operator<= (Temporal::samplepos_t s, Temporal::timecnt_t const & t) { TEMPORAL_DOMAIN_WARNING (Temporal::AudioTime); return s <= t.samples(); }
inline static bool operator<= (Temporal::Beats const & b, Temporal::timecnt_t const & t) { TEMPORAL_DOMAIN_WARNING (Temporal::BeatTime); return b <= t.beats(); }
inline static bool operator<= (Temporal::BBT_Time const & bbt, Temporal::timecnt_t const & t) { TEMPORAL_DOMAIN_WARNING (Temporal::BarTime); return bbt <= t.bbt(); }

inline static bool operator> (Temporal::samplepos_t s, Temporal::timecnt_t const & t) { TEMPORAL_DOMAIN_WARNING (Temporal::AudioTime); return s > t.samples(); }
inline static bool operator> (Temporal::Beats const & b, Temporal::timecnt_t const & t) { TEMPORAL_DOMAIN_WARNING (Temporal::BeatTime); return b > t.beats(); }
inline static bool operator> (Temporal::BBT_Time const & bbt, Temporal::timecnt_t const & t) { TEMPORAL_DOMAIN_WARNING (Temporal::BarTime); return bbt > t.bbt(); }

inline static bool operator>= (Temporal::samplepos_t s, Temporal::timecnt_t const & t) { TEMPORAL_DOMAIN_WARNING (Temporal::AudioTime); return s >= t.samples(); }
inline static bool operator>= (Temporal::Beats const & b, Temporal::timecnt_t const & t) { TEMPORAL_DOMAIN_WARNING (Temporal::BeatTime); return b >= t.beats(); }
inline static bool operator>= (Temporal::BBT_Time const & bbt, Temporal::timecnt_t const & t) { TEMPORAL_DOMAIN_WARNING (Temporal::BarTime); return bbt >= t.bbt(); }

namespace std {
	template<>
	struct numeric_limits<Temporal::timepos_t> {
		static Temporal::timepos_t lowest() {
			return Temporal::timepos_t (0);
		}

		static Temporal::timepos_t min() {
			return Temporal::timepos_t (0);
		}

		static Temporal::timepos_t max() {
			return Temporal::timepos_t (INT64_MAX);
		}
	};


	template<>
	struct numeric_limits<Temporal::timecnt_t> {
		static Temporal::timecnt_t lowest() {
			return Temporal::timecnt_t (0, Temporal::timepos_t());
		}

		static Temporal::timecnt_t min() {
			return Temporal::timecnt_t (0, Temporal::timepos_t());
		}

		static Temporal::timecnt_t max() {
			return Temporal::timecnt_t (INT64_MAX, Temporal::timepos_t());
		}

	};
}

#endif /* __libtemporal_timeline_h__ */
