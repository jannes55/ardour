/*
    Copyright (C) 2004 Paul Davis

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
#include <cmath>

#include "pbd/basename.h"

#include "ardour/quantize.h"
#include "ardour/midi_model.h"

#include "pbd/i18n.h"

using namespace std;
using namespace PBD;
using namespace ARDOUR;

/** Quantize notes
 *
 * grid parameters are the quantize value as beat divisors, ie 1.0 = quantize to beats,
 * 4 = quantize to beats/4, etc.
 */

Quantize::Quantize (bool snap_start, bool snap_end,
                    int start_grid, int end_grid,
                    float strength, float swing, Temporal::Beats const & threshold)
	: _snap_start (snap_start)
	, _snap_end (snap_end)
	, _start_grid(start_grid)
	, _end_grid(end_grid)
	, _strength (strength/100.0)
	, _swing (swing/100.0)
	, _threshold (threshold)
{
}

Quantize::~Quantize ()
{
}

static Temporal::Beats
swing_position (Temporal::Beats const & p, int grid_divisor, double swing, Temporal::Beats const & offset)
{
	Temporal::Beats pos (p);
	const Temporal::Beats grid_beats = Temporal::Beats (1, 0) / grid_divisor;
	const int32_t grid_ticks = Temporal::ticks_per_beat / grid_divisor;
	const int32_t pos_ticks = pos.to_ticks ();

	/* beats start out numbered at zero.
	 *
	 * every other position on the start-quantize-grid is
	 * optionally swung, meaning that its position is moved
	 * somewhere between its natural position and 2/3 of
	 * the way to the next start-quantize-grid position.
	 *
	 * so, if the _start grid is 0.5, the beat at 0 isn't
	 * swung, but something at 0.5 is, the beat at 1 isn't
	 * swung, but something at 1.5 is.
	 *
	 * if the start grid is 1, the beat at 0 isn't swung,
	 * but the beat at 1.0 is. the beat at 2.0 isn't swung,
	 * but the beat at 3.0 is. and so on.
	 *
	 * so the criterion for a position being swung is
	 * whether or not ((possible_grid_position / grid) % 2) != 0
	 */

	const bool swing_quantize_grid_position = (pos_ticks > 0)          && (pos_ticks % (2 * grid_ticks) != 0);
	const bool swing_previous_grid_position = (pos_ticks > grid_ticks) && ((pos_ticks - grid_ticks) % (2 * grid_ticks) != 0);

	/* one of these will not be subject to swing */

	Temporal::Beats swung_pos = pos;
	Temporal::Beats swung_previous_grid_position;

	if (pos_ticks > grid_ticks) {
		swung_previous_grid_position = pos - grid_beats;
	}

	if (swing_previous_grid_position) {
		swung_previous_grid_position = swung_previous_grid_position + ((2 * swing * grid_beats)/3);
	}

	if (swing_quantize_grid_position) {
		swung_pos = swung_pos + ((2 * swing * grid_beats)/3);
	}

	/* now correct for start-of-model offset */

	pos += offset;

	if ((pos - swung_pos).abs() > (pos - swung_previous_grid_position).abs()) {
		pos = swung_previous_grid_position;
	} else {
		pos = swung_pos;
	}

	return pos;
}

Command*
Quantize::operator () (boost::shared_ptr<MidiModel> model,
                       Temporal::Beats position,
                       std::vector<Evoral::Sequence<Temporal::Beats>::Notes>& seqs)
{
	/* Calculate offset from start of model to next closest quantize step,
	   to quantize relative to actual session beats (etc.) rather than from the
	   start of the model.
	*/
	const Temporal::Beats round_pos = (position / _start_grid) * _start_grid;
	const Temporal::Beats offset    = round_pos - position;

	MidiModel::NoteDiffCommand* cmd = new MidiModel::NoteDiffCommand (model, "quantize");

	for (std::vector<Evoral::Sequence<Temporal::Beats>::Notes>::iterator s = seqs.begin(); s != seqs.end(); ++s) {

		for (Evoral::Sequence<MidiModel::TimeType>::Notes::iterator i = (*s).begin(); i != (*s).end(); ++i) {

			/* compute new start + end points WITHOUT the offset
			 * caused by the start of the model (see above).
			 *
			 * these versions of new_start and new_end are
			 * guaranteed to precisely align with the quantize grid(s).
			 */

			Temporal::Beats new_start = (((*i)->time() - offset) / _start_grid) * _start_grid;
			Temporal::Beats new_end = (((*i)->end_time() - offset) / _end_grid) * _end_grid;

			if (_swing) {

				new_start = swing_position (new_start, _start_grid, _swing, offset);
				new_end = swing_position (new_end, _end_grid, _swing, offset);

			} else {

				/* now correct for start-of-model offset */

				new_start += offset;
				new_end += offset;
			}

			Temporal::Beats delta = new_start - (*i)->time();

			if (delta.abs() >= _threshold) {
				if (_snap_start) {
					delta = delta * _strength;
					cmd->change ((*i), MidiModel::NoteDiffCommand::StartTime,
					             (*i)->time() + delta);
				}
			}

			if (_snap_end) {
				delta = new_end - (*i)->end_time();

				if (delta.abs () >= _threshold) {

					Temporal::Beats new_dur (new_end - new_start);

					if (!new_dur) {
						new_dur = Temporal::Beats (_end_grid);
					}

					cmd->change ((*i), MidiModel::NoteDiffCommand::Length, new_dur);
				}
			}
		}
	}

	return cmd;
}
