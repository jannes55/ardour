/*
    Copyright (C) 2000-2011 Paul Davis

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

#include <string>
#include <gtkmm/enums.h>
#include "pbd/stacktrace.h"
#include "ardour/profile.h"

#include "canvas/debug.h"
#include "canvas/scroll_group.h"
#include "canvas/tracking_text.h"

#include "audio_clock.h"
#include "editor.h"
#include "editor_drag.h"
#include "main_clock.h"
#include "verbose_cursor.h"
#include "ardour_ui.h"
#include "ui_config.h"

#include "pbd/i18n.h"

using namespace std;
using namespace ARDOUR;

VerboseCursor::VerboseCursor (Editor* editor)
	: _editor (editor)
{
	_canvas_item = new ArdourCanvas::TrackingText (_editor->get_noscroll_group());
	CANVAS_DEBUG_NAME (_canvas_item, "verbose canvas cursor");
	_canvas_item->set_font_description (Pango::FontDescription (UIConfiguration::instance().get_LargerBoldFont()));
	color_handler ();

	UIConfiguration::instance().ColorsChanged.connect (sigc::mem_fun (*this, &VerboseCursor::color_handler));
}

void
VerboseCursor::color_handler ()
{
	_canvas_item->set_color (UIConfiguration::instance().color_mod ("verbose canvas cursor", "verbose canvas cursor"));
}

ArdourCanvas::Item *
VerboseCursor::canvas_item () const
{
	return _canvas_item;
}

/** Set the contents of the cursor.
 */
void
VerboseCursor::set (string const & text)
{
	_canvas_item->set (text);
}

void
VerboseCursor::show ()
{
	_canvas_item->show_and_track (true, true);
	_canvas_item->parent()->raise_to_top ();
}

void
VerboseCursor::hide ()
{
	_canvas_item->hide ();
	_canvas_item->parent()->lower_to_bottom ();
	/* reset back to a sensible default for the next time we display the VC */
	_canvas_item->set_offset (ArdourCanvas::Duple (10, 10));
}

void
VerboseCursor::set_offset (ArdourCanvas::Duple const & d)
{
	_canvas_item->set_offset (d);
}

void
VerboseCursor::set_time (timepos_t const & pos)
{
	char buf[128];
	Temporal::Time timecode;
	Temporal::BBT_Time bbt;

	if (_editor->_session == 0) {
		return;
	}

	/* Take clock mode from the primary clock */

	AudioClock::Mode m = ARDOUR_UI::instance()->primary_clock->mode();
	samplepos_t sample = pos.sample(); /* XXX fix me ... don't do this */

	switch (m) {
	case AudioClock::BBT:
		_editor->_session->bbt_time (sample, bbt);
		snprintf (buf, sizeof (buf), "%02" PRIu32 "|%02" PRIu32 "|%02" PRIu32, bbt.bars, bbt.beats, bbt.ticks);
		break;

	case AudioClock::Timecode:
		_editor->_session->timecode_time (sample, timecode);
		snprintf (buf, sizeof (buf), "%s", Temporal::timecode_format_time (timecode).c_str());
		break;

	case AudioClock::MinSec:
		AudioClock::print_minsec (sample, buf, sizeof (buf), _editor->_session->sample_rate());
		break;

	default:
		snprintf (buf, sizeof(buf), "%" PRIi64, sample);
		break;
	}

	_canvas_item->set (buf);
}

void
VerboseCursor::set_duration (timepos_t const & start, timepos_t const & end)
{
	char buf[128];
	Temporal::Time timecode;
	Temporal::BBT_Offset bbt;

	if (_editor->_session == 0) {
		return;
	}

	AudioClock::Mode m = ARDOUR_UI::instance()->primary_clock->mode ();

	switch (m) {
	case AudioClock::BBT:
	{
		timecnt_t d = _editor->_session->tempo_map().full_duration_at (start, start.distance (end), Temporal::BarTime);
		bbt = d.bbt();
		snprintf (buf, sizeof (buf), "%02" PRIu32 "|%02" PRIu32 "|%02" PRIu32, bbt.bars, bbt.beats, bbt.ticks);
		break;
	}

	case AudioClock::Timecode:
		_editor->_session->timecode_duration (start.distance (end).samples(), timecode);
		snprintf (buf, sizeof (buf), "%s", Temporal::timecode_format_time (timecode).c_str());
		break;

	case AudioClock::MinSec:
		AudioClock::print_minsec (end.sample() - start.sample(), buf, sizeof (buf), _editor->_session->sample_rate());
		break;

	default:
		snprintf (buf, sizeof(buf), "%" PRIi64, start.distance (end).samples());
		break;
	}

	_canvas_item->set (buf);
}

bool
VerboseCursor::visible () const
{
	return _canvas_item->visible();
}
