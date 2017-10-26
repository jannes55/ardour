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

#include <exception>
#include <sstream>

#include "pbd/enumwriter.h"
#include "pbd/compose.h"
#include "pbd/i18n.h"

#include "temporal/timeline.h"
#include "temporal/tempo.h"

using namespace Temporal;

timecnt_t timecnt_t::_max_timecnt (max_samplepos);

std::ostream&
std::operator<< (std::ostream & o, timecnt_t const & tc)
{
	return o << tc.to_string();
}

timecnt_t::timecnt_t (timepos_t const & tp)
	: _style (tp.lock_style())
{
	switch (_style) {
	case Temporal::AudioTime:
		_samples = tp.sample();
	case Temporal::BeatTime:
		_beats = tp.beats ();
	case Temporal::BarTime:
		_bbt = tp.bbt ();
	}
}

timecnt_t &
timecnt_t::operator= (timepos_t const & tp)
{
	_style = tp.lock_style();

	switch (_style) {
	case Temporal::AudioTime:
		_samples = tp.sample();
	case Temporal::BeatTime:
		_beats = tp.beats ();
	case Temporal::BarTime:
		_bbt = tp.bbt ();
	}

	return *this;
}

timecnt_t
timecnt_t::operator*(double d) const
{
	switch (_style) {
	case Temporal::AudioTime:
		return timecnt_t (samplepos_t (llrint (_samples * d)));
	case Temporal::BeatTime:
		return timecnt_t (_beats * d);
	default:
		break;
	}

	throw TemporalStyleException ("cannot multiply BBT time");
}

bool
timecnt_t::string_to (std::string const & str)
{
	if (isdigit (str[0])) {
		std::stringstream ss (str);
		ss >> _samples;
		_style = AudioTime;
		return true;
	}

	std::stringstream ss (str.substr (1));
	switch (str[0]) {
	case 'a':
		ss >> _samples;
		_style = AudioTime;
		return true;
		break;
	case 'b':
		ss >> _beats;
		_style = BeatTime;
		return true;
		break;
	case 'B':
		ss >> _bbt;
		_style = BarTime;
		return true;
	}

	throw TemporalTypeException (X_("unknown type character in timecnt_t string"));
}

std::string
timecnt_t::to_string () const
{
	switch (_style) {
	case Temporal::AudioTime:
		return string_compose ("a%1", _samples);
	case Temporal::BeatTime:
		return string_compose ("b%1", _beats);
	case Temporal::BarTime:
		return string_compose ("B%1", _bbt);
	}
	return std::string();
}

TempoMap* timepos_t::_tempo_map = 0;

/* special private constructor for max_timepos() */
timepos_t::timepos_t (PositionLockStatus /*ignored*/)
	: update_generation (-1)
	, _lock_status (PositionLockStatus (AudioTime))
	, _samplepos (max_samplepos)
{
	_beats = std::numeric_limits<Beats>::max ();
	_bbt = std::numeric_limits<BBT_Time>::max();
}

timepos_t timepos_t::_max_timepos (Temporal::AudioTime);

timepos_t::timepos_t()
	: update_generation (-1)
	, _lock_status (AudioTime, Dirty (BeatsDirty|BBTDirty))
	, _samplepos (0)
{
}

timepos_t::timepos_t (timecnt_t const & t)
	: update_generation (-1)
{
	(void) operator= (t);
}

timepos_t::timepos_t (samplepos_t s)
	: update_generation (-1)
	, _lock_status (AudioTime, Dirty (BeatsDirty|BBTDirty))
	, _samplepos (s)
{
}

timepos_t::timepos_t (Temporal::Beats const & b)
	: update_generation (-1)
	, _lock_status (BeatTime, Dirty (SampleDirty|BBTDirty))
	, _samplepos (b.to_ticks())
{
}

timepos_t::timepos_t (Temporal::BBT_Time const & bbt)
	: update_generation (-1)
	, _lock_status (BarTime, Dirty (SampleDirty|BeatsDirty))
	, _bbt (bbt)
{
}

timepos_t &
timepos_t::operator= (timecnt_t const & t)
{
	switch (t.style()) {
	case AudioTime:
		_samplepos = t.samples();
		if (_samplepos < 0) {
			throw TemporalStyleException ("negative sample timecnt used to construct timepos");
		}
		_lock_status = PositionLockStatus (AudioTime, Dirty (BeatsDirty|BBTDirty));
		break;
	case BeatTime:
		_beats = t.beats();
		if (_beats < Beats()) {
			throw TemporalStyleException ("negative beat timecnt used to construct timepos");
		}
		_lock_status = PositionLockStatus (BeatTime, Dirty (SampleDirty|BBTDirty));
		break;
	case BarTime:
		_bbt = BBT_Time (t.bbt().bars, t.bbt().beats, t.bbt().ticks);
		if (_bbt < BBT_Time()) {
			throw TemporalStyleException ("negative BBT timecnt used to construct timepos");
		}
		_lock_status = PositionLockStatus (BarTime, Dirty (BeatsDirty|SampleDirty));
		break;
	}
	return *this;
}

void
timepos_t::set_sample (samplepos_t s)
{
	_samplepos = s;
	_lock_status = PositionLockStatus (AudioTime, Dirty (BeatTime|BarTime));
}

void
timepos_t::set_beat (Beats const & b)
{
	_beats = b;
	_lock_status = PositionLockStatus (BeatTime, Dirty (AudioTime|BarTime));
}

void
timepos_t::set_bbt (BBT_Time const & bbt)
{
	_bbt = bbt;
	_lock_status = PositionLockStatus (BarTime, Dirty (BeatTime|AudioTime));
}

samplepos_t
timepos_t::sample () const
{
	switch (_lock_status.style()) {
	case AudioTime:
		break;
	case BeatTime:
		update_audio_and_bbt_times ();
	case BarTime:
		update_audio_and_beat_times ();
		break;
	}

	return _samplepos;
}

Temporal::Beats
timepos_t::beats () const
{
	switch (_lock_status.style()) {
	case AudioTime:
		update_music_times ();
		break;
	case BeatTime:
		break;
	case BarTime:
		update_audio_and_beat_times ();
		break;
	}

	return _beats;
}

Temporal::BBT_Time
timepos_t::bbt() const
{
	switch (_lock_status.style()) {
	case AudioTime:
		update_music_times ();
		break;
	case BeatTime:
		update_audio_and_bbt_times ();
		break;
	case BarTime:
		break;
	}

	return _bbt;
}

void
timepos_t::update_music_times () const
{
	update_generation = _tempo_map->update_music_times (update_generation, _samplepos, _beats, _bbt, _lock_status.dirty() & (BeatsDirty|BBTDirty));
}

void
timepos_t::update_audio_and_beat_times () const
{
	update_generation = _tempo_map->update_samples_and_beat_times (update_generation, _bbt, _samplepos, _beats, _lock_status.dirty() & (SampleDirty|BeatsDirty));
}

void
timepos_t::update_audio_and_bbt_times () const
{
	update_generation = _tempo_map->update_samples_and_bbt_times (update_generation, _beats, _samplepos, _bbt, _lock_status.dirty() & (SampleDirty|BBTDirty));
}

timepos_t
timepos_t::operator*(double d) const
{
	assert (d >= 0.0); /* do not allow a position to become negative via multiplication */
	switch (lock_style()) {
	case Temporal::AudioTime:
		return timepos_t (samplepos_t (llrint (_samplepos * d)));
	case Temporal::BeatTime:
		return timepos_t (beats() * d);
	default:
		break;
	}

	throw TemporalStyleException ("cannot multiply BBT time");
}

timepos_t &
timepos_t::operator*=(double d)
{
	assert (d >= 0.0); /* do not allow a position to become negative via multiplication */
	switch (lock_style()) {
	case Temporal::AudioTime:
		_samplepos = samplepos_t (llrint (_samplepos * d));
		return *this;
	case Temporal::BeatTime:
		_beats = _beats * d;
		return *this;
	default:
		break;
	}

	throw TemporalStyleException ("cannot multiply BBT time");
}

timepos_t
timepos_t::operator+(timepos_t d) const
{
	switch (_lock_status.style()) {
	case AudioTime:
		switch (d.lock_style()) {
		case AudioTime:
			return timepos_t (_samplepos + d.sample());
		case BeatTime:
			update_music_times ();
			return timepos_t (_beats + d.beats());
		case BarTime:
			return timepos_t (_tempo_map->bbt_walk (_bbt, d.bbt()));
		}
		break;
	case BeatTime:
		switch (d.lock_style()) {
		case AudioTime:
			update_audio_and_bbt_times ();
			return timepos_t (_samplepos + d.sample());
		case BeatTime:
			return timepos_t (_beats + d.beats());
		case BarTime:
			update_music_times ();
			return timepos_t (_tempo_map->bbt_walk (_bbt, d.bbt()));
		}
		break;
	case BarTime:
		switch (d.lock_style()) {
		case AudioTime:
			update_audio_and_beat_times ();
			return timepos_t (_samplepos + d.sample());
		case BeatTime:
			update_music_times ();
			return timepos_t (_beats + d.beats());
		case BarTime:
			return timepos_t (_tempo_map->bbt_walk (_bbt, d.bbt()));
		}
		break;
	}
	/*NOTREACHED*/
	return timepos_t (*this);
}

timepos_t
timepos_t::operator- (timepos_t d) const
{
	switch (_lock_status.style()) {
	case AudioTime:
		switch (d.lock_style()) {
		case AudioTime:
			return timepos_t (_samplepos + d.sample());
		case BeatTime:
			update_music_times ();
			return timepos_t (_beats + d.beats());
		case BarTime:
			return timepos_t (_tempo_map->bbt_walk (_bbt, -BBT_Offset (d.bbt())));
		}
		break;
	case BeatTime:
		switch (d.lock_style()) {
		case AudioTime:
			update_audio_and_bbt_times ();
			return timepos_t (_samplepos + d.sample());
		case BeatTime:
			return timepos_t (_beats + d.beats());
		case BarTime:
			update_music_times ();
			return timepos_t (_tempo_map->bbt_walk (_bbt, -BBT_Offset (d.bbt())));
		}
		break;
	case BarTime:
		switch (d.lock_style()) {
		case AudioTime:
			update_audio_and_beat_times ();
			return timepos_t (_samplepos + d.sample());
		case BeatTime:
			update_music_times ();
			return timepos_t (_beats + d.beats());
		case BarTime:
			return timepos_t (_tempo_map->bbt_walk (_bbt, -BBT_Offset (d.bbt())));
		}
		break;
	}
	/*NOTREACHED*/
	return timepos_t (*this);
}

/* */

timepos_t
timepos_t::operator+ (samplepos_t s) const
{
	switch (_lock_status.style()) {
	case AudioTime:
		break;
	case BeatTime:
		update_audio_and_bbt_times ();
		break;
	case BarTime:
		update_audio_and_beat_times ();
		break;
	}

	return timepos_t (_samplepos + s);
}

timepos_t
timepos_t::operator+ (Temporal::Beats const & b) const
{
	switch (_lock_status.style()) {
	case AudioTime:
		update_music_times ();
		break;
	case BeatTime:
		break;
	case BarTime:
		update_audio_and_beat_times ();
		break;
	}

	return timepos_t (_beats + b);
}

timepos_t
timepos_t::operator+ (Temporal::BBT_Offset const & bbt) const
{
	switch (_lock_status.style()) {
	case AudioTime:
		update_music_times ();
		break;
	case BeatTime:
		update_audio_and_bbt_times ();
		break;
	case BarTime:
		break;
	}

	_bbt = _tempo_map->bbt_walk (_bbt, BBT_Offset (bbt));
	return *this;
}

timepos_t
timepos_t:: operator- (samplepos_t s) const
{
	switch (_lock_status.style()) {
	case AudioTime:
		break;
	case BeatTime:
		update_audio_and_bbt_times ();
		break;
	case BarTime:
		update_audio_and_beat_times ();
		break;
	}

	return timepos_t (_samplepos - s);
}

timepos_t
timepos_t::operator- (Temporal::Beats const & b) const
{
	switch (_lock_status.style()) {
	case AudioTime:
		update_music_times ();
		break;
	case BeatTime:
		break;
	case BarTime:
		update_audio_and_beat_times ();
		break;
	}

	return timepos_t (_beats - b);
}

timepos_t
timepos_t:: operator- (Temporal::BBT_Offset const & bbt) const
{
	switch (_lock_status.style()) {
	case AudioTime:
		update_music_times ();
		break;
	case BeatTime:
		update_audio_and_bbt_times ();
		break;
	case BarTime:
		break;
	}

	return timepos_t (_tempo_map->bbt_walk (_bbt, -bbt));
}

/* */

timepos_t &
timepos_t:: operator+= (samplepos_t s)
{
	switch (_lock_status.style()) {
	case AudioTime:
		break;
	case BeatTime:
		update_audio_and_bbt_times ();
		break;
	case BarTime:
		update_audio_and_beat_times ();
		break;
	}

	_samplepos -= s;
	return *this;
}

timepos_t &
timepos_t::operator+=(Temporal::Beats const & b)
{
	switch (_lock_status.style()) {
	case AudioTime:
		update_music_times ();
		break;
	case BeatTime:
		break;
	case BarTime:
		update_audio_and_beat_times ();
		break;
	}

	_beats += b;
	return *this;
}

timepos_t &
timepos_t:: operator+=(Temporal::BBT_Offset const & bbt)
{
	switch (_lock_status.style()) {
	case AudioTime:
		update_music_times ();
		break;
	case BeatTime:
		update_audio_and_bbt_times ();
		break;
	case BarTime:
		break;
	}

	_bbt = _tempo_map->bbt_walk (_bbt, bbt);
	return *this;
}

timepos_t &
timepos_t:: operator-= (samplepos_t s)
{
	switch (_lock_status.style()) {
	case AudioTime:
		break;
	case BeatTime:
		update_audio_and_bbt_times ();
		break;
	case BarTime:
		update_audio_and_beat_times ();
		break;
	}

	_samplepos -= s;
	return *this;
}

timepos_t &
timepos_t::operator-=(Temporal::Beats const & b)
{
	switch (_lock_status.style()) {
	case AudioTime:
		update_music_times ();
		break;
	case BeatTime:
		break;
	case BarTime:
		update_audio_and_beat_times ();
		break;
	}

	_beats -= b;
	return *this;
}

timepos_t &
timepos_t:: operator-=(Temporal::BBT_Offset const & bbt)
{
	switch (_lock_status.style()) {
	case AudioTime:
		update_music_times ();
		break;
	case BeatTime:
		update_audio_and_bbt_times ();
		break;
	case BarTime:
		break;
	}

	_bbt = _tempo_map->bbt_walk (_bbt, -bbt);
	return *this;
}

/* */

timepos_t
timepos_t::operator+(timecnt_t d) const
{
	switch (d.style()) {
	case Temporal::AudioTime:
		return operator+ (d.samples());
	case Temporal::BeatTime:
		return operator+ (d.beats());
	case Temporal::BarTime:
		return operator+ (d.bbt());
	}
	/*NOTREACHED*/
	return timepos_t (0);
}

timepos_t
timepos_t::operator-(timecnt_t d) const
{
	switch (d.style()) {
	case Temporal::AudioTime:
		return operator- (d.samples());
	case Temporal::BeatTime:
		return operator- (d.beats());
	case Temporal::BarTime:
		return operator- (d.bbt());
	}
	/*NOTREACHED*/
	return timepos_t (0);
}

timepos_t &
timepos_t::operator+=(timecnt_t d)
{
	switch (d.style()) {
	case Temporal::AudioTime:
		return operator+= (d.samples());
	case Temporal::BeatTime:
		return operator+= (d.beats());
	case Temporal::BarTime:
		return operator+= (d.bbt());
	}
	/*NOTREACHED*/
	return *this;
}

timepos_t &
timepos_t::operator-=(timecnt_t d)
{
	switch (d.style()) {
	case Temporal::AudioTime:
		return operator-= (d.samples());
	case Temporal::BeatTime:
		return operator-= (d.beats());
	case Temporal::BarTime:
		return operator-= (d.bbt());
	}
	/*NOTREACHED*/
	return *this;
}

std::ostream&
std::operator<< (std::ostream & o, timepos_t const & tp)
{
	return o << tp.to_string();
}

std::string
timepos_t::to_string () const
{
	switch (_lock_status.style()) {
	case AudioTime:
		return string_compose ("a%1", _samplepos);
	case BeatTime:
		return string_compose ("b%1", _beats);
	case BarTime:
		return string_compose ("B%1", _bbt);
	}
	/*NOTREACHED*/
	return std::string();
}

bool
timepos_t::string_to (std::string const & str)
{
	using std::string;
	using std::cerr;
	using std::endl;

	samplepos_t s;
	Beats beats;
	BBT_Time bbt;

	if (isdigit (str[0])) {
		/* old school position format: we assume samples */
		std::stringstream ss (str);
		ss >> s;
		set_sample (s);
		cerr << "deserialized timepos from older " << str << " as " << *this << endl;
		return true;
	}

	std::stringstream ss (str.substr (1));

	switch (str[0]) {
	case 'a':
		ss >> s;
		set_sample (s);
		cerr << "deserialized timepos from " << str << " as " << *this << endl;
		return true;
	case 'b':
		ss >> beats;
		set_beat (beats);
		cerr << "deserialized timepos from " << str << " as " << *this << endl;
		return true;
	case 'B':
		ss >> bbt;
		set_bbt (bbt);
		cerr << "deserialized timepos from " << str << " as " << *this << endl;
		return true;
	}

	std::cerr << "Unknown timepos string representation \"" << str << "\"" << std::endl;

	return false;
}

void
timepos_t::set_lock_style (LockStyle ls)
{
	_lock_status.set_style (ls);
}
