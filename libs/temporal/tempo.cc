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

#include "pbd/i18n.h"

#include "temporal/tempo.h"

using namespace Temporal;
using std::cerr;
using std::cout;
using std::endl;
using Temporal::superclock_t;

bool
Tempo::set_ramped (bool)
{
#warning implement Tempo::set_ramped
	return true;
}

bool
Tempo::set_clamped (bool)
{
#warning implement Tempo::set_clamped
	return true;
}

/* overloaded operator* that avoids floating point math when multiplying a superclock position by a number of quarter notes */
superclock_t operator*(superclock_t sc, Temporal::Beats const & b) { return (sc * ((b.get_beats() * Temporal::ticks_per_beat) + b.get_ticks())) / Temporal::ticks_per_beat; }

double
Meter::samples_per_grid (const Tempo& tempo, samplecnt_t sr) const
{
	/* This is tempo- and meter-sensitive. The number it returns
	   is based on the interval between any two divisions defined by this
	   Meter at the given Tempo.

	   The return value IS NOT interpretable in terms of "beats".
	*/

	return (60.0 * sr) / (tempo.note_types_per_minute() * (_note_value / tempo.note_type()));
}

double
Meter::samples_per_bar (const Tempo& tempo, samplecnt_t sr) const
{
	return samples_per_grid (tempo, sr) * _divisions_per_bar;
}


Temporal::BBT_Time
Meter::bbt_add (Temporal::BBT_Time const & bbt, Temporal::BBT_Offset const & add) const
{
	int32_t bars = bbt.bars;
	int32_t beats = bbt.beats;
	int32_t ticks = bbt.ticks;

	if ((bars ^ add.bars) < 0) {
		/* signed-ness varies */
		if (abs(add.bars) >= abs(bars)) {
			/* addition will change which side of "zero" the answer is on;
			   adjust bbt.bars towards zero to deal with "unusual" BBT math
			*/
			if (bars < 0) {
				bars++;
			} else {
				bars--;
			}
		}
	}

	if ((beats ^ add.beats) < 0) {
		/* signed-ness varies */
		if (abs (add.beats) >= abs (beats)) {
			/* adjust bbt.beats towards zero to deal with "unusual" BBT math */
			if (beats < 0) {
				beats++;
			} else {
				beats--;
			}
		}
	}

	Temporal::BBT_Offset r (bars + add.bars, beats + add.beats, ticks + add.ticks);

	if (r.ticks >= Temporal::Beats::PPQN) {
		r.beats += r.ticks / Temporal::Beats::PPQN;
		r.ticks %= Temporal::Beats::PPQN;
	}

	if (r.beats > _divisions_per_bar) {
		r.bars += r.beats / _divisions_per_bar;
		r.beats %= _divisions_per_bar;
	}

	if (r.beats == 0) {
		r.beats = 1;
	}

	if (r.bars == 0) {
		r.bars = 1;
	}

	return Temporal::BBT_Time (r.bars, r.beats, r.ticks);
}

Temporal::BBT_Time
Meter::bbt_subtract (Temporal::BBT_Time const & bbt, Temporal::BBT_Offset const & sub) const
{
	int32_t bars = bbt.bars;
	int32_t beats = bbt.beats;
	int32_t ticks = bbt.ticks;

	if ((bars ^ sub.bars) < 0) {
		/* signed-ness varies */
		if (abs (sub.bars) >= abs (bars)) {
			/* adjust bbt.bars towards zero to deal with "unusual" BBT math */
			if (bars < 0) {
				bars++;
			} else {
				bars--;
			}
		}
	}

	if ((beats ^ sub.beats) < 0) {
		/* signed-ness varies */
		if (abs (sub.beats) >= abs (beats)) {
			/* adjust bbt.beats towards zero to deal with "unusual" BBT math */
			if (beats < 0) {
				beats++;
			} else {
				beats--;
			}
		}
	}

	Temporal::BBT_Offset r (bars - sub.bars, beats - sub.beats, ticks - sub.ticks);

	if (r.ticks < 0) {
		r.beats -= 1 - (r.ticks / Temporal::Beats::PPQN);
		r.ticks = Temporal::Beats::PPQN + (r.ticks % Temporal::Beats::PPQN);
	}

	if (r.beats <= 0) {
		r.bars -= 1 - (r.beats / _divisions_per_bar);
		r.beats = _divisions_per_bar + (r.beats % _divisions_per_bar);
	}

	if (r.beats == 0) {
		r.beats = 1;
	}

	if (r.bars <= 0) {
		r.bars -= 1;
	}

	return Temporal::BBT_Time (r.bars, r.beats, r.ticks);
}

Temporal::BBT_Offset
Meter::bbt_delta (Temporal::BBT_Time const & a, Temporal::BBT_Time const & b) const
{
	return Temporal::BBT_Offset (a.bars - b.bars, a.beats - b.beats, a.ticks - b.ticks);
}

Temporal::BBT_Time
Meter::round_to_bar (Temporal::BBT_Time const & bbt) const
{
	Temporal::BBT_Time b = bbt.round_up_to_beat ();
	if (b.beats > _divisions_per_bar/2) {
		b.bars++;
	}
	b.beats = 1;
	return b;
}


Temporal::Beats
Meter::to_quarters (Temporal::BBT_Offset const & offset) const
{
	Temporal::Beats b;

	b += (offset.bars * _divisions_per_bar * 4) / _note_value;
	b += (offset.beats * 4) / _note_value;
	b += Temporal::Beats::ticks (offset.ticks);

	return b;
}

superclock_t
TempoMetric::superclock_per_note_type_at_superclock (superclock_t sc) const
{
	return superclocks_per_note_type () * expm1 (_c_per_superclock * sc);
}

superclock_t
TempoMetric::superclocks_per_grid (samplecnt_t sr) const
{
	return (superclock_ticks_per_second * Meter::note_value()) / (note_types_per_minute() / Tempo::note_type());
}

superclock_t
TempoMetric::superclocks_per_bar (samplecnt_t sr) const
{
	return superclocks_per_grid (sr) * _divisions_per_bar;
}

/*
Ramp Overview

      |                     *
Tempo |                   *
Tt----|-----------------*|
Ta----|--------------|*  |
      |            * |   |
      |         *    |   |
      |     *        |   |
T0----|*             |   |
  *   |              |   |
      _______________|___|____
      time           a   t (next tempo)
      [        c         ] defines c

Duration in beats at time a is the integral of some Tempo function.
In our case, the Tempo function (Tempo at time t) is
T(t) = T0(e^(ct))

>>1/S(t) = (1/S0)(e^ct) => (1/S)(t) = (e^(ct))/S0 => S(t) = S0/(e^(ct))

with function constant
c = log(Ta/T0)/a

>>c = log ((1/Sa)/(1/S0)) / a => c = log (S0/Sa) / a

so
a = log(Ta/T0)/c

>>a = log ((1/Ta)/(1/S0) / c => a = log (S0/Sa) / c

The integral over t of our Tempo function (the beat function, which is the duration in beats at some time t) is:
b(t) = T0(e^(ct) - 1) / c

>>b(t) = 1/S0(e^(ct) - 1) / c  => b(t) = (e^(ct) - 1) / (c * S0)

To find the time t at beat duration b, we use the inverse function of the beat function (the time function) which can be shown to be:
t(b) = log((c.b / T0) + 1) / c

>>t(b) = log((c*b / (1/S0)) + 1) / c => t(b) = log ((c*b * S0) + 1) / c

The time t at which Tempo T occurs is a as above:
t(T) = log(T / T0) / c

>> t(1/S) = log ((1/S) / (1/S0) /c => t(1/S) = log (S0/S) / c

The beat at which a Tempo T occurs is:
b(T) = (T - T0) / c

>> b(1/S) = (1/S - 1/S0) / c

The Tempo at which beat b occurs is:
T(b) = b.c + T0

>> T(b) = b.c + (1/S0)

We define c for this tempo ramp by placing a new tempo section at some time t after this one.
Our problem is that we usually don't know t.
We almost always know the duration in beats between this and the new section, so we need to find c in terms of the beat function.
Where a = t (i.e. when a is equal to the time of the next tempo section), the beat function reveals:
t = b log (Ta / T0) / (T0 (e^(log (Ta / T0)) - 1))

By substituting our expanded t as a in the c function above, our problem is reduced to:
c = T0 (e^(log (Ta / T0)) - 1) / b

>> c = (1/S0) (e^(log ((1/Sa) / (1/S0))) - 1) / b => c = (1/S0) (e^(log (S0/Sa)) - 1) / b => c (e^(log (S0/Sa)) - 1) / (b * S0)

Of course the word 'beat' has been left loosely defined above.
In music, a beat is defined by the musical pulse (which comes from the tempo)
and the meter in use at a particular time (how many  pulse divisions there are in one bar).
It would be more accurate to substitute the work 'pulse' for 'beat' above.

 */

/* equation to compute c is:
 *
 *    c = log (Ta / T0) / a
 *
 * where
 *
 *   a : time into section (from section start
 *  T0 : tempo at start of section
 *  Ta : tempo at time a into section
 *
 * THE UNITS QUESTION
 *
 * log (Ta / T0) / (time-units) => C is in per-time-units (1/time-units)
 *
 * so the question is what are the units of a, and thus c?
 *
 * we could use ANY time units (because we can measure a in any time units)
 * but whichever one we pick dictates how we can use c in the future since
 * all subsequent computations will need to use the same time units.
 *
 * options:
 *
 *    pulses        ... whole notes, possibly useful, since we can use it for any other note_type
 *    quarter notes ... linearly related to pulses
 *    beats         ... not a fixed unit of time
 *    minutes       ... linearly related to superclocks
 *    samples       ... needs sample rate
 *    superclocks   ... frequently correct
 *
 * so one answer might be to compute c in two different units so that we have both available.
 *
 * hence, compute_c_superclocks() and compute_c_pulses()
 */

void
TempoMetric::compute_c_superclock (samplecnt_t sr, superclock_t end_scpqn, superclock_t superclock_duration)
{
	if ((superclocks_per_quarter_note() == end_scpqn) || !ramped()) {
		_c_per_superclock = 0.0;
		return;
	}

	_c_per_superclock = log ((double) superclocks_per_quarter_note () / end_scpqn) / superclock_duration;
}
void
TempoMetric::compute_c_quarters (samplecnt_t sr, superclock_t end_scpqn, Temporal::Beats const & quarter_duration)
{
	if ((superclocks_per_quarter_note () == end_scpqn) || !ramped()) {
		_c_per_quarter = 0.0;
		return;
	}

	_c_per_quarter = log (superclocks_per_quarter_note () / (double) end_scpqn) /  quarter_duration.to_double();
}

superclock_t
TempoMetric::superclock_at_qn (Temporal::Beats const & qn) const
{
	if (_c_per_quarter == 0.0) {
		/* not ramped, use linear */
		return llrint (superclocks_per_quarter_note () * qn.to_double());
	}

	return llrint (superclocks_per_quarter_note() * (log1p (_c_per_quarter * qn.to_double()) / _c_per_quarter));
}

timepos_t
TempoMapPoint::time() const
{
	switch (_map->time_domain()) {
	case AudioTime:
		return timepos_t (sample());
	case BeatTime:
	case BarTime:
		return timepos_t (bbt());
	}
	/*NOTREACHED*/
	abort();
	/*NOTREACHED*/
	return timepos_t();
}

samplepos_t
TempoMapPoint::sample() const
{
	return superclock_to_samples (_sclock, _map->sample_rate());
}

void
TempoMapPoint::set_map (TempoMap* m)
{
	_map = m;
}

void
TempoMapPoint::set_dirty (bool yn)
{
	if (yn != _dirty) {
		_dirty = yn;
		if (yn && _map) {
			_map->set_dirty (true);
		}
	}
}

Temporal::Beats
TempoMapPoint::quarters_at (superclock_t sc) const
{
	/* This TempoMapPoint must already have a fully computed metric and position */

	if (!ramped()) {
		return _quarters + Temporal::Beats ((sc - _sclock) / (double) (metric().superclocks_per_quarter_note ()));
	}

	return _quarters + Temporal::Beats (expm1 (metric().c_per_superclock() * (sc - _sclock)) / (metric().c_per_superclock() * metric().superclocks_per_quarter_note ()));
}

Temporal::Beats
TempoMapPoint::quarters_at (Temporal::BBT_Time const & bbt) const
{
	/* This TempoMapPoint must already have a fully computed metric and position */

	Temporal::BBT_Offset offset = metric().bbt_delta (bbt, _bbt);
	return _quarters + metric().to_quarters (offset);
}

Temporal::BBT_Time
TempoMapPoint::bbt_at (Temporal::Beats const & qn) const
{
	/* This TempoMapPoint must already have a fully computed metric and position */

	Temporal::Beats quarters_delta = qn - _quarters;
	int32_t ticks_delta = quarters_delta.to_ticks (Temporal::Beats::PPQN);
	return metric().bbt_add (_bbt, Temporal::BBT_Offset (0, 0,  ticks_delta));
}

Temporal::BBT_Time
TempoMapPoint::bbt_at (samplepos_t pos) const
{
	/* This TempoMapPoint must already have a fully computed metric and position */

	superclock_t sclock_delta = sclock() - samples_to_superclock (pos, _map->sample_rate());
	int32_t ticks_delta = sclock_delta / metric().superclocks_per_ppqn ();
	return metric().bbt_add (_bbt, Temporal::BBT_Offset (0, 0,  ticks_delta));
}

/* TEMPOMAP */

TempoMap::TempoMap (Tempo const & initial_tempo, Meter const & initial_meter, samplecnt_t sr)
	: _sample_rate (sr)
	, _dirty (false)
	, _generation (0) // XXX needs to be reloaded from saved state??
{
	TempoMapPoint tmp (this, TempoMapPoint::Flag (TempoMapPoint::ExplicitMeter|TempoMapPoint::ExplicitTempo), initial_tempo, initial_meter, 0, Temporal::Beats(), Temporal::BBT_Time(), AudioTime);
	_points.push_back (tmp);
}

TempoMap::~TempoMap()
{
}

#define S2Sc(s) (samples_to_superclock ((s), _sample_rate))

void
TempoMap::set_dirty (bool yn)
{
	_dirty = yn;
}

Meter const &
TempoMap::meter_at (timepos_t const & time) const
{
	switch (time.lock_style()) {
	case AudioTime:
		return meter_at (time.sample());
		break;
	case BarTime:
		return meter_at (time.bbt());
		break;
	case BeatTime:
		return meter_at (time.beats());
		break;
	}
	/*NOTREACHED*/
	return meter_at (0);
}

Meter const &
TempoMap::meter_at (samplepos_t s) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return meter_at_locked (S2Sc (s));
}

Meter const &
TempoMap::meter_at (Temporal::Beats const & b) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return meter_at_locked (b);
}

Meter const &
TempoMap::meter_at (Temporal::BBT_Time const & bbt) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return meter_at_locked (bbt);
}

Tempo const &
TempoMap::tempo_at (timepos_t const & time) const
{
	switch (time.lock_style()) {
	case AudioTime:
		return tempo_at (time.sample());
		break;
	case BarTime:
		return tempo_at (time.bbt());
		break;
	case BeatTime:
		return tempo_at (time.beats());
		break;
	}
	/*NOTREACHED*/
	return tempo_at (0);
}

Tempo const &
TempoMap::tempo_at (samplepos_t s) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return tempo_at_locked (S2Sc(s));
}

Tempo const &
TempoMap::tempo_at (Temporal::Beats const &b) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return tempo_at_locked (b);
}

Tempo const &
TempoMap::tempo_at (Temporal::BBT_Time const & bbt) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return tempo_at_locked (bbt);
}

void
TempoMap::full_rebuild ()
{
	rebuild (_points.back().sclock());
}

void
TempoMap::rebuild (superclock_t limit)
{
	Glib::Threads::RWLock::WriterLock lm (_lock);

	/* step one: remove all implicit points after a dirty explicit point */

  restart:
	TempoMapPoints::iterator point = _points.begin();
	TempoMapPoints::iterator first_explicit_dirty = _points.end();

	while ((point != _points.end()) && (point->is_implicit() || (point->is_explicit() && !point->dirty ()))) {
		++point;
	}

	first_explicit_dirty = point;

	if (first_explicit_dirty == _points.end()) {
		/* nothing is dirty */
		return;
	}

	/* remove all implicit points, because we're going to recalculate them all */

	while (point != _points.end()) {
		TempoMapPoints::iterator next = point;
		++next;

		if (point->is_implicit()) {
			_points.erase (point);
		}

		point = next;
	}

	/* compute C-by-quarters for all ramped sections, because we need it shortly */

	for (point = first_explicit_dirty; point != _points.end(); ) {
		TempoMapPoints::iterator nxt = point;
		++nxt;

		if (point->ramped() && (nxt != _points.end())) {
			point->compute_c_quarters (_sample_rate, nxt->metric().superclocks_per_quarter_note (), nxt->quarters() - point->quarters());
		}

		point = nxt;
	}

	TempoMapPoints::iterator prev = _points.end();

	if (_time_domain != Temporal::AudioTime) {
		/* Compute correct quarter-note and superclock times based on
		 * music times
		 */

		for (point = first_explicit_dirty; point != _points.end(); ) {

			TempoMapPoints::iterator next = point;
			++next;

			if (prev != _points.end()) {
				/* determine superclock and quarter note time for this (music-time) locked point */

				Temporal::Beats qn = prev->quarters_at (point->bbt());
				superclock_t sc = prev->sclock() + prev->metric().superclock_at_qn (qn - prev->quarters());

				if (qn != point->quarters() || point->sclock() != sc) {
					point->set_quarters (qn);
					point->set_sclock (sc);
					_points.sort (TempoMapPoint::SuperClockComparator());
					goto restart;
				}
			}

			prev = point;
			point = next;
		}
	}

	/* _points is guaranteed sorted in superclock and quarter note order. It may not be sorted BBT order because of re-ordering
	 * of music-time locked points.
	 */

	prev = _points.end();

	/* step two: add new implicit points between each pair of explicit
	 * points, after the dirty explicit point
	 */

	bool hit_dirty = false;
	superclock_t first_dirty = 0;

	for (point = _points.begin(); point != _points.end(); ) {

		if (!hit_dirty) {
			if (!point->dirty()) {
				++point;
				continue;
			}
			hit_dirty = true;
			first_dirty = point->sclock();
		}

		TempoMapPoints::iterator next = point;
		++next;

		if (_time_domain == AudioTime) {
			if (prev != _points.end()) {
				/* audio-locked time domain: recompute point's BBT and quarter-note position since this may have changed */
				point->set_quarters (prev->quarters_at (point->sclock()));
				if (static_cast<Meter>(point->metric()) != static_cast<Meter>(prev->metric())) {
					/* new meter, must be on bar/measure start */
					point->set_bbt (prev->bbt_at (point->quarters()).round_up_to_bar ());
				} else {
					/* no meter change, tempo change required to be on beat */
					point->set_bbt (prev->bbt_at (point->quarters()).round_up_to_beat());
				}
			}
		}

		superclock_t sc = point->sclock();
		Temporal::Beats qn (point->quarters ());
		Temporal::BBT_Time bbt (point->bbt());
		const bool ramped = point->ramped () && next != _points.end();

		/* Temporal::Beats are really quarter notes. This counts how many quarter notes
		   there are between grid points in this section of the tempo map.
		 */
		const Temporal::Beats qn_step = (Temporal::Beats (1,0) * 4) / point->metric().note_value();

		/* compute implicit points as far as the next explicit point, or limit,
		   whichever comes first.
		*/

		const superclock_t sc_limit = (next == _points.end() ? limit : (*next).sclock());

		while (1) {

			/* define next beat in superclocks, beats and bbt */

			qn += qn_step;
			bbt = point->metric().bbt_add (bbt, Temporal::BBT_Offset (0, 1, 0));

			if (!ramped) {
				sc += point->metric().superclocks_per_note_type();
			} else {
				sc = point->sclock() + point->metric().superclock_at_qn (qn - point->quarters());
			}

			if (sc >= sc_limit) {
				break;
			}

			_points.insert (next, TempoMapPoint (*point, sc, qn, bbt));
		}

		point->set_dirty (false);
		prev = point;
		point = next;
	}

	//_update_generation++;
	Changed (first_dirty, _points.back().sclock()); /* EMIT SIGNAL */
}

bool
TempoMap::set_tempo_and_meter (Tempo const & tempo, Meter const & meter, samplepos_t s, bool ramp, bool flexible)
{
	/* CALLER MUST HOLD LOCK */

	assert (!_points.empty());

	superclock_t sc = S2Sc (s);

	/* special case: first map entry is later than the new point */

	if (_points.front().sclock() > sc) {
		/* first point is later than sc. There's no iterator to reference a point at or before sc */

		/* determine beats and BBT time for this new tempo point. Note that tempo changes (points) must be deemed to be on beat,
		   even if the user moves them later. Even after moving, the TempoMapPoint that was beat N is still beat N, and is not
		   fractional.
		*/

		Temporal::Beats b = _points.front().quarters_at (sc).round_to_beat();
		Temporal::BBT_Time bbt = _points.front().bbt_at (b).round_to_beat ();

		_points.insert (_points.begin(), TempoMapPoint (this, TempoMapPoint::ExplicitTempo, tempo, meter, sc, b, bbt, ramp));
		set_dirty (true);
		return true;
	}

	/* special case #3: only one map entry, at the same time as the new point.
	   This is the common case when editing tempo/meter in a session with a single tempo/meter
	*/

	if (_points.size() == 1 && _points.front().sclock() == sc) {
		/* change metrics */
		*((Tempo*) &_points.front().nonconst_metric()) = tempo;
		*((Meter*) &_points.front().nonconst_metric()) = meter;
		_points.front().make_explicit (TempoMapPoint::Flag (TempoMapPoint::ExplicitTempo|TempoMapPoint::ExplicitMeter));
		set_dirty (true);
		return true;
	}

	/* Remember: iterator_at() returns an iterator that references the TempoMapPoint at or BEFORE sc */

	TempoMapPoints::iterator i = iterator_at (sc);
	TempoMapPoints::iterator nxt = i;
	++nxt;

	if (i->sclock() == sc) {
		/* change  metrics */
		*((Tempo*) &i->nonconst_metric()) = tempo;
		*((Meter*) &i->nonconst_metric()) = meter;
		i->make_explicit (TempoMapPoint::Flag (TempoMapPoint::ExplicitTempo|TempoMapPoint::ExplicitMeter));
		set_dirty (true); /* XXX is this required? */
		/* done */
		return true;
	}

	if (!flexible && (sc - i->sclock() < i->metric().superclocks_per_note_type())) {
		cerr << "new tempo too close to previous ...\n";
		return false;
	}

	TempoMapPoints::iterator e (i);
	while (!e->is_explicit() && e != _points.begin()) {
		--e;
	}

	if (e->metric().ramped()) {
		/* need to adjust ramp constants for preceding explict point, since the new point will be positioned right after it
		   and thus defines the new ramp distance.
		*/
		e->compute_c_superclock (_sample_rate, tempo.superclocks_per_quarter_note (), sc);
	}

	/* determine beats and BBT time for this new tempo point. Note that tempo changes (points) must be deemed to be on beat,
	   even if the user moves them later. Even after moving, the TempoMapPoint that was beat N is still beat N, and is not
	   fractional.
	 */

	Temporal::Beats qn = i->quarters_at (sc).round_to_beat();

	/* rule: all Tempo changes must be on-beat. So determine the nearest later beat to "sc"
	 */

	Temporal::BBT_Time bbt = i->bbt_at (qn).round_up_to_beat ();

	/* Modify the iterator to reference the point AFTER this new one, because STL insert is always "insert-before"
	 */

	if (i != _points.end()) {
		++i;
	}

	_points.insert (i, TempoMapPoint (this, TempoMapPoint::ExplicitTempo, tempo, meter, sc, qn, bbt, ramp));
	set_dirty (true);

	return true;
}

bool
TempoMap::set_tempo (Tempo const & t, samplepos_t s, bool ramp)
{
	Glib::Threads::RWLock::WriterLock lm (_lock);

	assert (!_points.empty());

	superclock_t sc = S2Sc (s);

	/* special case: first map entry is later than the new point */

	if (_points.front().sclock() > sc) {
		/* first point is later than sc. There's no iterator to reference a point at or before sc */

		/* determine beats and BBT time for this new tempo point. Note that tempo changes (points) must be deemed to be on beat,
		   even if the user moves them later. Even after moving, the TempoMapPoint that was beat N is still beat N, and is not
		   fractional.
		*/

		Temporal::Beats b = _points.front().quarters_at (sc).round_to_beat();
		Temporal::BBT_Time bbt = _points.front().bbt_at (b).round_to_beat ();

		_points.insert (_points.begin(), TempoMapPoint (this, TempoMapPoint::ExplicitTempo, t, _points.front().metric(), sc, b, bbt, ramp));
		return true;
	}

	/* special case #2: only one map entry, at the same time as the new point.
	   This is the common case when editing tempo/meter in a session with a single tempo/meter
	*/

	if (_points.size() == 1 && _points.front().sclock() == sc) {
		/* change tempo */
		*((Tempo*) &_points.front().nonconst_metric()) = t;
		_points.front().make_explicit (TempoMapPoint::ExplicitTempo);
		return true;
	}

	/* Remember: iterator_at() returns an iterator that references the TempoMapPoint at or BEFORE sc */

	TempoMapPoints::iterator i = iterator_at (sc);
	TempoMapPoints::iterator nxt = i;
	++nxt;

	if (i->sclock() == sc) {
		/* new time for tempo is same as the existing one, so just change tempo */
		*((Tempo*) &i->nonconst_metric()) = t;
		i->make_explicit (TempoMapPoint::ExplicitTempo);
		/* done */
		return true;
	}

	/* new time differs from existing point */

	if (sc - i->sclock() < i->metric().superclocks_per_note_type()) {
		/* can't put a new tempo change less than 1
		   note-of-previous-meter from the previous note.
		*/
		cerr << "new tempo too close to previous ...\n";
		return false;
	}

	Meter const & meter (i->metric());

	/* walk backwards to first prior explicit point */

	TempoMapPoints::iterator prev_explicit (i);

	while (!prev_explicit->is_explicit() && prev_explicit != _points.begin()) {
		--prev_explicit;
	}

	if (prev_explicit->metric().ramped()) {
		/* need to adjust ramp constants for preceding explict point, since the new point will be positioned right after it
		   and thus defines the new ramp distance.
		*/
		prev_explicit->compute_c_superclock (_sample_rate, t.superclocks_per_quarter_note (), sc);
	}

	/* determine beats and BBT time for this new tempo point. Note that tempo changes (points) must be deemed to be on beat,
	   even if the user moves them later. Even after moving, the TempoMapPoint that was beat N is still beat N, and is not
	   fractional.
	 */

	Temporal::Beats qn = i->quarters_at (sc).round_to_beat();

	/* rule: all Tempo changes must be on-beat. So determine the nearest later beat to "sc"
	 */

	Temporal::BBT_Time bbt = i->bbt_at (qn).round_up_to_beat ();

	/* Modify the iterator to reference the point AFTER this new one, because STL insert is always "insert-before"
	 */

	if (i != _points.end()) {
		++i;
	}
	_points.insert (i, TempoMapPoint (this, TempoMapPoint::ExplicitTempo, t, meter, sc, qn, bbt, ramp));
	return true;
}

bool
TempoMap::set_tempo (Tempo const & t, timepos_t const & time, bool ramp)
{
	switch (time.lock_style()) {
	case AudioTime:
		return set_tempo (t, time.sample(), ramp);
		break;
	case BarTime:
		return set_tempo (t, time.bbt(), ramp);
		break;
	case BeatTime:
		cerr << (_("programming error: beat time used in tempo map as current")) << endl;
		abort ();
		break;
	}
	/*NOTREACHED*/
	return false;
}

bool
TempoMap::set_tempo (Tempo const & t, Temporal::BBT_Time const & bbt, bool ramp)
{
	Glib::Threads::RWLock::WriterLock lm (_lock);

	/* tempo changes are required to be on-beat */

	Temporal::BBT_Time on_beat = bbt.round_up_to_beat();

	assert (!_points.empty());

	if (_points.front().bbt() > on_beat) {
		BBT_Time b = _points.front().bbt();
		BBT_Time const & bra (bbt);
		BBT_Time const & brb (b);
		cerr << "Cannot insert tempo at " << bra << " before first point at " << brb << endl;
		return false;
	}

	if (_points.size() == 1 && _points.front().bbt() == on_beat) {
		/* change Tempo */
		*((Tempo*) &_points.front().nonconst_metric()) = t;
		_points.front().make_explicit (TempoMapPoint::ExplicitTempo);
		return true;
	}

	TempoMapPoints::iterator i = iterator_at (on_beat);

	if (i->bbt() == on_beat) {
		*((Tempo*) &i->nonconst_metric()) = t;
		i->make_explicit (TempoMapPoint::ExplicitTempo);
		return true;
	}

	Meter const & meter (i->metric());
	++i;

	_points.insert (i, TempoMapPoint (this, TempoMapPoint::ExplicitTempo, t, meter, 0, Temporal::Beats(), on_beat, ramp));

	return true;
}

void
TempoMap::remove_tempo_at (TempoMapPoint const & p)
{
	/* this doesn't remove the point @param p, but it does make it now
	 * refer to the same tempo as the preceding point, and makes it
	 * non-explicit tempo.
	 */

	Glib::Threads::RWLock::WriterLock lm (_lock);

	if (_points.empty()) {
		return;
	}

	TempoMapPoints::iterator i;
	TempoMapPoints::iterator prev = _points.end();

	for (i = _points.begin(); i != _points.end(); ++i) {
		if (&p == &(*i)) {
			break;
		}
		prev = i;
	}

	if (i == _points.end()) {
		return;
	}

	/* change tempo record here */
	*((Tempo*) &(i->nonconst_metric())) = *((Tempo*) &prev->metric());
	/* make it inexplicit */
	i->set_flags (TempoMapPoint::Flag (i->flags() & ~TempoMapPoint::ExplicitTempo));
}

bool
TempoMap::set_meter (Meter const & m, timepos_t const & time)
{
	switch (time.lock_style()) {
	case AudioTime:
		return set_meter (m, time.sample());
		break;
	case BarTime:
		return set_meter (m, time.bbt());
		break;
	case BeatTime:
		cerr << (_("programming error: beat time used in tempo map as current")) << endl;
		abort ();
		break;
	}
	/*NOTREACHED*/
	return false;
}

bool
TempoMap::set_meter (Meter const & m, Temporal::BBT_Time const & bbt)
{
	Glib::Threads::RWLock::WriterLock lm (_lock);
	Temporal::BBT_Time measure_start (bbt.round_up_to_bar ());

	assert (!_points.empty());

	if (_points.front().bbt() > measure_start) {
		cerr << "Cannot insert meter at " << bbt << " before first point at " << _points.front().bbt() << endl;
		return false;
	}

	if (_points.size() == 1 && _points.front().bbt() == measure_start) {
		/* change Meter */
		*((Meter*) &_points.front().nonconst_metric()) = m;
		_points.front().make_explicit (TempoMapPoint::ExplicitMeter);
		return true;
	}

	TempoMapPoints::iterator i = iterator_at (measure_start);

	if (i->bbt() == measure_start) {
		*((Meter*) &i->nonconst_metric()) = m;
		i->make_explicit (TempoMapPoint::ExplicitMeter);
		return true;
	}

	Temporal::Beats qn = i->quarters_at (measure_start);
	superclock_t sc = i->sclock() + i->metric().superclock_at_qn (qn);

	Tempo const & tempo (i->metric());
	++i;

	_points.insert (i, TempoMapPoint (this, TempoMapPoint::ExplicitMeter, tempo, m, sc, qn, measure_start, BeatTime));
	return true;
}

bool
TempoMap::set_meter (Meter const & m, samplepos_t s)
{
	Glib::Threads::RWLock::WriterLock lm (_lock);
	assert (!_points.empty());

	superclock_t sc = S2Sc (s);

	/* special case #2: first map entry is later than the new point */

	if (_points.front().sclock() > sc) {
		/* determine quarters and BBT time for this new tempo point. Note that tempo changes (points) must be deemed to be on beat,
		   even if the user moves them later. Even after moving, the TempoMapPoint that was beat N is still beat N, and is not
		   fractional.
		*/

		Temporal::Beats b = _points.front().quarters_at (sc).round_to_beat();
		Temporal::BBT_Time bbt = _points.front().bbt_at (b).round_to_beat ();

		_points.insert (_points.begin(), TempoMapPoint (this, TempoMapPoint::ExplicitMeter, _points.front().metric(), m, sc, b, bbt, AudioTime));
		return true;
	}

	/* special case #3: only one map entry, at the same time as the new point.

	   This is the common case when editing tempo/meter in a session with a single tempo/meter
	*/

	if (_points.size() == 1 && _points.front().sclock() == sc) {
		/* change meter */
		*((Meter*) &_points.front().nonconst_metric()) = m;
		_points.front().make_explicit (TempoMapPoint::ExplicitMeter);
		return true;
	}

	TempoMapPoints::iterator i = iterator_at (sc);

	if (i->sclock() == sc) {
		/* change meter */
		*((Meter*) &i->nonconst_metric()) = m;

		/* enforce rule described below regarding meter change positions */

		if (i->bbt().beats != 1) {
			i->set_bbt (Temporal::BBT_Time (i->bbt().bars + 1, 1, 0));
		}

		i->make_explicit (TempoMapPoint::ExplicitMeter);

		return true;
	}

	if (sc - i->sclock() < i->metric().superclocks_per_note_type()) {
		cerr << "new tempo too close to previous ...\n";
		return false;
	}

	/* determine quarters and BBT time for this new tempo point. Note that tempo changes (points) must be deemed to be on beat,
	   even if the user moves them later. Even after moving, the TempoMapPoint that was beat N is still beat N, and is not
	   fractional.
	*/

	Temporal::Beats b = i->quarters_at (sc).round_to_beat();

	/* rule: all Meter changes must start a new measure. So determine the nearest, lower beat to "sc". If this is not
	   the first division of the measure, move to the next measure.
	 */

	Temporal::BBT_Time bbt = i->bbt_at (b).round_down_to_beat ();

	if (bbt.beats != 1) {
		bbt.bars += 1;
		bbt.beats = 1;
		bbt.ticks = 0;
	}

	Tempo const & tempo (i->metric());
	++i;

	_points.insert (i, TempoMapPoint (this, TempoMapPoint::ExplicitMeter, tempo, m, sc, b, bbt, AudioTime));
	return true;
}

void
TempoMap::remove_meter_at (TempoMapPoint const & p)
{
	/* this doesn't remove the point @param p, but it does make it now
	 * refer to the same tempo as the preceding point, and makes it
	 * non-explicit tempo.
	 */

	Glib::Threads::RWLock::WriterLock lm (_lock);

	if (_points.empty()) {
		return;
	}

	TempoMapPoints::iterator i;
	TempoMapPoints::iterator prev = _points.end();

	for (i = _points.begin(); i != _points.end(); ++i) {
		if (&p == &(*i)) {
			break;
		}
		prev = i;
	}

	if (i == _points.end()) {
		return;
	}

	/* change meter record here */
	*((Meter*) &(i->nonconst_metric())) = *((Meter*) &prev->metric());
	/* make it inexplicit */
	i->set_flags (TempoMapPoint::Flag (i->flags() & ~TempoMapPoint::ExplicitMeter));
}

TempoMapPoints::iterator
TempoMap::iterator_at (superclock_t sc)
{
	/* CALLER MUST HOLD LOCK */

	if (_points.empty()) {
		throw EmptyTempoMapException();
	}

	if (_points.size() == 1) {
		return _points.begin();
	}

	/* Construct an arbitrary TempoMapPoint. The only property we care about is it's superclock time,
	   so other values used in the constructor are arbitrary and irrelevant.
	*/

	TempoMetric const & metric (_points.front().metric());
	const TempoMapPoint tp (this, TempoMapPoint::Flag (0), metric, metric, sc, Temporal::Beats(), Temporal::BBT_Time(), AudioTime);
	TempoMapPoint::SuperClockComparator scmp;

	TempoMapPoints::iterator tmp = upper_bound (_points.begin(), _points.end(), tp, scmp);

	if (tmp != _points.begin()) {
		return --tmp;
	}

	return tmp;
}

TempoMapPoints::iterator
TempoMap::iterator_at (Temporal::Beats const & qn)
{
	/* CALLER MUST HOLD LOCK */

	if (_points.empty()) {
		throw EmptyTempoMapException();
	}

	if (_points.size() == 1) {
		return _points.begin();
	}

	/* Construct an arbitrary TempoMapPoint. The only property we care about is its quarters time,
	   so other values used in the constructor are arbitrary and irrelevant.
	*/

	TempoMetric const & metric (_points.front().metric());
	const TempoMapPoint tp (this, TempoMapPoint::Flag (0), metric, metric, 0, qn, Temporal::BBT_Time(), AudioTime);
	TempoMapPoint::QuarterComparator bcmp;

	TempoMapPoints::iterator tmp = upper_bound (_points.begin(), _points.end(), tp, bcmp);

	if (tmp != _points.begin()) {
		return --tmp;
	}

	return tmp;
}

TempoMapPoints::iterator
TempoMap::iterator_at (Temporal::BBT_Time const & bbt)
{
	/* CALLER MUST HOLD LOCK */

	if (_points.empty()) {
		throw EmptyTempoMapException();
	}

	if (_points.size() == 1) {
		return _points.begin();
	}

	/* Construct an arbitrary TempoMapPoint. The only property we care about is its bbt time,
	   so other values used in the constructor are arbitrary and irrelevant.
	*/

	TempoMetric const & metric (_points.front().metric());
	const TempoMapPoint tp (this, TempoMapPoint::Flag(0), metric, metric, 0, Temporal::Beats(), bbt, BarTime);
	TempoMapPoint::BBTComparator bcmp;

	TempoMapPoints::iterator tmp = upper_bound (_points.begin(), _points.end(), tp, bcmp);

	if (tmp != _points.begin()) {
		return --tmp;
	}

	return tmp;
}

Temporal::BBT_Time
TempoMap::bbt_at (samplepos_t s) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return bbt_at_locked (S2Sc (s));
}

Temporal::BBT_Time
TempoMap::bbt_at_locked (superclock_t sc) const
{
	TempoMapPoint point (const_point_at (sc));
	Temporal::Beats b ((sc - point.sclock()) / (double) point.metric().superclocks_per_quarter_note());
	return point.metric().bbt_add (point.bbt(), Temporal::BBT_Offset (0, b.get_beats(), b.get_ticks()));
}

Temporal::BBT_Time
TempoMap::bbt_at (Temporal::Beats const & qn) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return bbt_at_locked (qn);
}

Temporal::BBT_Time
TempoMap::bbt_at_locked (Temporal::Beats const & qn) const
{
	TempoMapPoint const & point (const_point_at (qn));
	Temporal::Beats delta (qn - point.quarters());
	return point.metric().bbt_add (point.bbt(), Temporal::BBT_Offset (0, delta.get_beats(), delta.get_ticks()));
}

samplepos_t
TempoMap::sample_at (Temporal::Beats const & qn) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return sample_at_locked (qn);
}

samplepos_t
TempoMap::sample_at_locked (Temporal::Beats const & qn) const
{
	TempoMapPoints::const_iterator i = const_iterator_at (qn);

	/* compute distance from reference point to b. Remember that Temporal::Beats is always interpreted as quarter notes */

	const Temporal::Beats q_delta = qn - i->quarters();
	superclock_t sclock_delta = i->metric().superclock_at_qn (q_delta);
	return superclock_to_samples (i->sclock() + sclock_delta, _sample_rate);
}

samplepos_t
TempoMap::sample_at (Temporal::BBT_Time const & bbt) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return sample_at_locked (bbt);
}

samplepos_t
TempoMap::sample_at_locked (BBT_Time const & bbt) const
{
	TempoMapPoints::const_iterator i = const_iterator_at (bbt);

	/* this computes the distance from the point, in beats whose size is
	   determined by the meter.
	*/

	const Temporal::BBT_Offset delta = i->metric().bbt_delta (bbt, i->bbt());

	/* convert to quarter notes */
	const int32_t ticks = delta.ticks + (Temporal::Beats::PPQN * delta.beats * 4) / i->metric().note_value();

	/* get distance in superclocks */
	return superclock_to_samples (i->sclock() + i->metric().superclock_at_qn (Temporal::Beats::ticks (ticks)), _sample_rate);
}

samplepos_t
TempoMap::samplepos_plus_bbt (samplepos_t pos, BBT_Time op) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	BBT_Time pos_bbt = bbt_at (pos);

	pos_bbt.ticks += op.ticks;
	if (pos_bbt.ticks >= ticks_per_beat) {
		++pos_bbt.beats;
		pos_bbt.ticks -= ticks_per_beat;
	}
	pos_bbt.beats += op.beats;

	double divisions_per_bar = meter_at_locked (pos_bbt).divisions_per_bar();
	while (pos_bbt.beats >= divisions_per_bar + 1) {
		++pos_bbt.bars;
		divisions_per_bar = meter_at_locked (pos_bbt).divisions_per_bar();
		pos_bbt.beats -= divisions_per_bar;
	}
	pos_bbt.bars += op.bars;

	return sample_at (pos_bbt);
}

/** Count the number of beats that are equivalent to distance when going forward,
    starting at pos.
*/
Temporal::Beats
TempoMap::samplewalk_to_quarters (samplepos_t pos, samplecnt_t distance) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	TempoMapPoint const & first (const_point_at (pos));
	TempoMapPoint const & last (const_point_at (pos+distance));
	Temporal::Beats a = first.quarters_at (S2Sc (pos));
	Temporal::Beats b = last.quarters_at (S2Sc (pos+distance));
	return b - a;
}

Temporal::Beats
TempoMap::samplewalk_to_quarters (Temporal::Beats const & pos, samplecnt_t distance) const
{
	/* XXX this converts from beats to samples and back to beats... undesirable */
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	samplepos_t s = const_point_at (pos).sample_at (pos);
	s += distance;
	return const_point_at (distance).quarters_at (s);

}

Temporal::Beats
TempoMap::bbtwalk_to_quarters (Beats const & pos, BBT_Offset const & distance) const
{
	return quarter_note_at (bbt_walk (bbt_at (pos), distance)) - pos;
}

void
TempoMap::set_sample_rate (samplecnt_t new_sr)
{
	Glib::Threads::RWLock::WriterLock lm (_lock);
	double ratio = new_sr / (double) _sample_rate;

	for (TempoMapPoints::iterator i = _points.begin(); i != _points.end(); ++i) {
		i->map_reset_set_sclock_for_sr_change (llrint (ratio * i->sclock()));
	}
}

void
TempoMap::dump (std::ostream& ostr)
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	dump_locked (ostr);
}

void
TempoMap::dump_locked (std::ostream& ostr)
{
	ostr << "\n\n------------\n";
	for (TempoMapPoints::iterator i = _points.begin(); i != _points.end(); ++i) {
		ostr << *i << std::endl;
	}
}

void
TempoMap::remove_explicit_point (samplepos_t s)
{
	Glib::Threads::RWLock::WriterLock lm (_lock);
	superclock_t sc = S2Sc (s);
	TempoMapPoints::iterator p = iterator_at (sc);

	if (p->sclock() == sc) {
		_points.erase (p);
		set_dirty (true);
	}
}

bool
TempoMap::move_to (timepos_t const & cur, timepos_t const & dest, bool push)
{
	Glib::Threads::RWLock::WriterLock lm (_lock);

	superclock_t current = S2Sc (cur.sample());
	superclock_t destination;

	TempoMapPoints::iterator p;

	switch (cur.lock_style()) {
	case AudioTime:
		p = iterator_at (cur.sample());
		break;
	case BarTime:
		p = iterator_at (cur.bbt());
		break;
	case BeatTime:
		cerr << (_("programming error: beat time used in tempo map as current")) << endl;
		abort ();
		break;
	}

	if (p->sclock() != current) {
		cerr << "No point @ " << current << " (time domain was " << cur.lock_style() << ')' << endl;
		return false;
	}

	switch (dest.lock_style()) {
	case AudioTime:
	case BarTime:
		dest.update_audio_and_beat_times ();
		break;
	case BeatTime:
		cerr << (_("programming error: beat time used in tempo map as destination")) << endl;
		abort ();
	}

	destination = S2Sc (dest.sample ());

	/* put a "dirty" flag in at the nearest (prior) explicit point to the removal point
	 */

	if (p != _points.begin()) {
		TempoMapPoints::iterator prev (p);
		--prev;
		while (prev != _points.begin() && !prev->is_explicit()) {
			--prev;
		}
		prev->set_dirty (true);
	} else {
		TempoMapPoints::iterator nxt (p);
		++nxt;
		while (nxt != _points.end() && !nxt->is_explicit()) {
			++nxt;
		}
		if (nxt != _points.end()) {
			nxt->set_dirty (true);
		}
	}

	if (!set_tempo_and_meter (p->metric(), p->metric(), destination, p->ramped(), true)) {
		return false;
	}

	if (push) {

		p = iterator_at (destination);
		++p;

		const superclock_t delta = destination - current;

		while (p != _points.end()) {
			p->set_sclock (p->sclock() + delta);
			++p;
		}
	}

	_points.erase (p);
	set_dirty (true);

	return true;
}

void
TempoMap::get_points (TempoMapPoints& ret) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	for (TempoMapPoints::const_iterator p = _points.begin(); p != _points.end(); ++p) {
		if (p->is_explicit()) {
			ret.push_back (*p);
		}
	}
}

void
TempoMap::get_tempos (TempoMapPoints& ret) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	for (TempoMapPoints::const_iterator p = _points.begin(); p != _points.end(); ++p) {
		if (p->is_explicit_tempo()) {
			ret.push_back (*p);
		}
	}
}

void
TempoMap::get_meters (TempoMapPoints& ret) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	for (TempoMapPoints::const_iterator p = _points.begin(); p != _points.end(); ++p) {
		if (p->is_explicit_meter()) {
			ret.push_back (*p);
		}
	}
}

void
TempoMap::get_grid (TempoMapPoints& ret, samplepos_t s, samplepos_t e, Temporal::Beats const & resolution)
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	superclock_t start = S2Sc (s);
	superclock_t end = S2Sc (e);

	TempoMapPoints::iterator p = iterator_at (start);

	while (p != _points.end() && p->sclock() < start) {
		++p;
	}

	if (resolution == Temporal::Beats()) {
		/* just hand over the points as-is */
		while (p != _points.end() && p->sclock() < end) {
			ret.push_back (*p);
			++p;
		}
		return;
	}

	superclock_t pos = p->sclock();
	Temporal::Beats qpos;
	TempoMapPoints::iterator nxt = p;
	++nxt;

	while ((p != _points.end()) && (pos < end)) {
		/* recompute grid down to @param resolution level
		 */

		superclock_t sclock_delta = p->metric().superclock_at_qn (qpos);

		ret.push_back (TempoMapPoint (this, TempoMapPoint::Flag (TempoMapPoint::ExplicitMeter|TempoMapPoint::ExplicitTempo),
		                              p->metric(), p->metric(),
		                              p->sclock() + sclock_delta,
		                              p->quarters() + qpos,
		                              p->metric().bbt_add (p->bbt(), Temporal::BBT_Offset (0, qpos.get_beats(), qpos.get_ticks())),
		                              p->ramped()));

		qpos += resolution;
		pos += sclock_delta;

		if (pos >= nxt->sclock()) {
			p = nxt;
			++nxt;
		}
	}
}


void
TempoMap::get_bar_grid (TempoMapPoints& ret, samplepos_t s, samplepos_t e, int32_t bar_gap)
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	superclock_t start = S2Sc (s);
	superclock_t end = S2Sc (e);

	for (TempoMapPoints::iterator p = iterator_at (start); (p != _points.end()) && (p->sclock() < end); ++p) {

		if ((p->sclock() >= start) && (p->bbt().beats == 1) && ((p->bbt().bars == 1) || (p->bbt().bars % bar_gap == 0))) {
			ret.push_back (TempoMapPoint (this, TempoMapPoint::Flag (TempoMapPoint::ExplicitMeter|TempoMapPoint::ExplicitTempo),
			                              p->metric(), p->metric(),
			                              p->sclock(),
			                              p->quarters(),
			                              p->bbt(),
			                              p->ramped()));
		}
	}
}

std::ostream&
std::operator<<(std::ostream& str, Meter const & m)
{
	return str << m.divisions_per_bar() << '/' << m.note_value();
}

std::ostream&
std::operator<<(std::ostream& str, Tempo const & t)
{
	return str << t.note_types_per_minute() << " 1/" << t.note_type() << " notes per minute (" << t.superclocks_per_note_type() << " sc-per-1/" << t.note_type() << ')';
}

std::ostream&
std::operator<<(std::ostream& str, TempoMapPoint const & tmp)
{
	str << '@' << std::setw (12) << tmp.sclock() << ' ' << tmp.sclock() / (double) superclock_ticks_per_second
	    << (tmp.is_explicit_tempo() ? " EXP-T" : " imp-t")
	    << (tmp.is_explicit_meter() ? " EXP-M" : " imp-m")
	    << " qn " << tmp.quarters ()
	    << " bbt " << tmp.bbt()
		;

	if (tmp.is_explicit()) {
		str << " tempo " << *((Tempo const *) &tmp.metric())
		    << " meter " << *((Meter const *) &tmp.metric())
			;
	}

	if (tmp.is_explicit() && tmp.ramped()) {
		str << " ramp c/sc = " << tmp.metric().c_per_superclock() << " c/qn " << tmp.metric().c_per_quarter();
	}
	return str;
}

samplepos_t
TempoMap::sample_plus_quarters_as_samples (samplepos_t start, Temporal::Beats const & distance) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	TempoMapPoints::const_iterator i = const_iterator_at (start);

	const Temporal::Beats start_qn = i->quarters_at (start);
	const Temporal::Beats end_qn = start_qn + distance;

	TempoMapPoints::const_iterator e = const_iterator_at (end_qn);

	return superclock_to_samples (e->metric().superclock_at_qn (end_qn - e->quarters()), _sample_rate);
}

Temporal::Beats
TempoMap::sample_delta_as_quarters (samplepos_t start, samplepos_t distance) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return quarter_note_at (start + distance) - quarter_note_at (start);
}

Temporal::superclock_t
TempoMap::sample_quarters_delta_as_samples (samplepos_t start, Temporal::Beats const & distance) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	const Temporal::Beats start_qn = const_iterator_at (start)->quarters_at (S2Sc (start));
	return sample_at_locked (start_qn + distance);
}

int
TempoMap::update_music_times (int generation, samplepos_t pos, Temporal::Beats & b, Temporal::BBT_Time & bbt, bool force)
{
	if (_dirty) {
		full_rebuild ();
	}

	Glib::Threads::RWLock::ReaderLock lm (_lock);

	if (!force && (generation == _generation)) {
		return _generation;
	}

	TempoMapPoints::iterator i = iterator_at (pos);

	const superclock_t sc = samples_to_superclock (pos, _sample_rate);
	b = i->quarters_at (sc);
	bbt = i->bbt_at (sc);

	return _generation;
}

int
TempoMap::update_samples_and_bbt_times (int generation, Temporal::Beats const & b, samplepos_t & pos, Temporal::BBT_Time & bbt, bool force)
{
	if (_dirty) {
		full_rebuild ();
	}

	Glib::Threads::RWLock::ReaderLock lm (_lock);

	if (!force && (generation == _generation)) {
		return _generation;
	}

	TempoMapPoints::iterator i = iterator_at (b);

	pos = sample_at (b);
	bbt = i->bbt_at (b);

	return _generation;
}

int
TempoMap::update_samples_and_beat_times (int generation, Temporal::BBT_Time const & bbt, samplepos_t & pos, Temporal::Beats & b, bool force)
{
	if (_dirty) {
		full_rebuild ();
	}

	Glib::Threads::RWLock::ReaderLock lm (_lock);

	if (!force && (generation == _generation)) {
		return _generation;
	}

	TempoMapPoints::iterator i = iterator_at (bbt);

	pos = sample_at (bbt);
	b = i->quarters_at (bbt);

	return _generation;
}

samplecnt_t
TempoMap::samples_per_quarter_note_at (samplepos_t samples) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return superclock_to_samples (const_iterator_at (samples_to_superclock (samples, _sample_rate))->metric().superclocks_per_quarter_note (), _sample_rate);
}

BBT_Time
TempoMap::bbt_walk (BBT_Time const & bbt, BBT_Offset const & offset) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	// TempoMapPoint const & start (const_point_at (bbt));

#warning TempoMap::bbt_walk() is not yet implemented

	/* common case: start + finish are both defined by the same TempoMetric */

	/* uncommon case: at least one tempo and/or meter change between start
	 * and finish ... have to walk.
	 */
	return BBT_Time ();
}

Temporal::Beats
TempoMap::quarter_note_at (timepos_t const & pos) const
{
	switch (pos.lock_style()) {
	case BeatTime:
		return pos.beats();
	case AudioTime:
		return quarter_note_at (pos.sample());
	default:
		break;
	}

	return quarter_note_at (pos.bbt());
}

Temporal::Beats
TempoMap::quarter_note_at (Temporal::BBT_Time const & bbt) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return const_iterator_at (bbt)->quarters_at (bbt);
}

Temporal::Beats
TempoMap::quarter_note_at (samplepos_t pos) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return const_iterator_at (pos)->quarters_at (S2Sc (pos));
}

XMLNode&
TempoMap::get_state ()
{
	XMLNode* node = new XMLNode (X_("TempoMap"));
	return *node;
}

int
TempoMap::set_state (XMLNode const & /*node*/, int /*version*/)
{
	return 0;
}

bool
TempoMap::can_remove (Tempo const & t) const
{
	return !is_initial (t);
}

bool
TempoMap::is_initial (Tempo const & t) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	assert (!_points.empty());
	return &t == dynamic_cast<Tempo const *> (&_points.front().metric());
}

bool
TempoMap::is_initial (Meter const & m) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	assert (!_points.empty());
	return &m == dynamic_cast<Meter const *> (&_points.front().metric());
}


bool
TempoMap::can_remove (Meter const & m) const
{
	return !is_initial (m);
}

/** returns the sample duration of the supplied BBT time at a specified sample position in the tempo map.
 * @param pos the frame position in the tempo map.
 * @param bbt the distance in BBT time from pos to calculate.
 * @param dir the rounding direction..
 * @return the duration in frames between pos and bbt
*/
samplecnt_t
TempoMap::bbt_duration_at (samplepos_t pos, const BBT_Time& bbt, int /* dir_ignored */ ) const
{
	return full_duration_at (pos, timecnt_t (bbt), AudioTime).samples();
}

/** Takes a duration (in any time domain) and considers it as a distance from the given position.
 *  Returns a distance in the requested domain, taking tempo changes into account.
 *
 *  Obviously, if the given distance is in the same time domain as the requested domain,
 *  the returned distance is identical to the given one.
 */

timecnt_t
TempoMap::full_duration_at (timepos_t const & pos, timecnt_t const & duration, LockStyle domain) const
{
	timepos_t p (pos);

	if (domain == duration.style()) {
		return duration;
	}

	switch (domain) {
	case AudioTime:
		switch (duration.style()) {
		case AudioTime:
			/*NOTREACHED*/
			break;
		case BeatTime:
			p.update_audio_and_beat_times(); /* XXX optimize by just fetching beats */
			p += duration;
			p.update_audio_and_beat_times(); /* XXX optimize by just fetching beats */
			return timecnt_t (p.sample() - pos.sample());
			break;
		case BarTime:
			/* we're not doing this yet, if ever */
			abort ();
			/*NOTREACHED*/
			break;
		}
		break;
	case BeatTime:
		switch (duration.style()) {
		case AudioTime:
			p.update_music_times ();
			p += duration;
			p.update_music_times ();
			return timecnt_t (p.beats () - pos.beats());
			break;
		case BeatTime:
			/*NOTREACHED*/
			break;
		case BarTime:
			/* we're not doing this yet, if ever */
			abort ();
			/*NOTREACHED*/
			break;
		}
		break;
	case BarTime:
		/* we're not doing this yet, if ever */
		abort ();
		/*NOTREACHED*/
		break;
	}
	/*NOTREACHED*/
	abort ();
	/*NOTREACHED*/
	return timecnt_t();

}

Tempo const *
TempoMap::next_tempo (Tempo const & t) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	TempoMapPoints::const_iterator p = _points.begin();

	while (p != _points.end()) {
		if (&t == &p->metric()) {
			break;
		}
		++p;
	}

	if (p != _points.end()) {
		++p;

		if (p != _points.end()) {
			return &p->metric();
		}
	}

	return 0;
}

uint32_t
TempoMap::n_meters () const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	uint32_t n = 0;

	for (TempoMapPoints::const_iterator p = _points.begin(); p != _points.end(); ++p) {
		if (p->is_explicit_meter()) {
			++n;
		}
	}

	return n;
}

uint32_t
TempoMap::n_tempos () const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	uint32_t n = 0;

	for (TempoMapPoints::const_iterator p = _points.begin(); p != _points.end(); ++p) {
		if (p->is_explicit_tempo()) {
			++n;
		}
	}

	return n;
}

void
TempoMap::insert_time (timepos_t const & pos, timecnt_t const & duration)
{
	Glib::Threads::RWLock::WriterLock lm (_lock);
	TempoMapPoints::iterator i;

	switch (duration.style()) {
	case AudioTime:
		i = iterator_at (S2Sc (pos.sample()));
		if (i->sample() < pos.sample()) {
			++i;
		}

		while (i != _points.end()) {
			i->set_sclock (S2Sc (i->sample() + duration.samples()));
			++i;
		}
		break;
	case BeatTime:
		i = iterator_at (pos.beats());
		if (i->quarters() < pos.beats()) {
			++i;
		}

		while (i != _points.end()) {
			i->set_quarters (i->quarters() + duration.beats());
			++i;
		}
		break;
	case BarTime:
		i = iterator_at (pos.bbt());
		if (i->bbt() < pos.bbt()) {
			++i;
		}

		while (i != _points.end()) {
			i->set_bbt (bbt_walk (i->bbt(), duration.bbt()));
			++i;
		}
		break;
	}
}

bool
TempoMap::remove_time (timepos_t const & pos, timecnt_t const & duration)
{
	Glib::Threads::RWLock::WriterLock lm (_lock);
	TempoMapPoints::iterator i;

	bool moved = false;

	switch (duration.style()) {
	case AudioTime:
		i = iterator_at (S2Sc (pos.sample()));
		if (i->sample() < pos.sample()) {
			++i;
		}

		while (i != _points.end()) {
			i->set_sclock (S2Sc (i->sample() - duration.samples()));
			++i;
			moved = true;
		}
		break;
	case BeatTime:
		i = iterator_at (pos.beats());
		if (i->quarters() < pos.beats()) {
			++i;
		}

		while (i != _points.end()) {
			i->set_quarters (i->quarters() - duration.beats());
			++i;
			moved = true;
		}
		break;
	case BarTime:
		i = iterator_at (pos.bbt());
		if (i->bbt() < pos.bbt()) {
			++i;
		}

		while (i != _points.end()) {
			i->set_bbt (bbt_walk (i->bbt(), -duration.bbt()));
			++i;
			moved = true;
		}
		break;
	}

	return moved;
}
