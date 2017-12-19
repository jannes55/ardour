/*
    Copyright (C) 2000 Paul Davis

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

#ifndef __gtk_ardour_editor_cursors_h__
#define __gtk_ardour_editor_cursors_h__

#include "pbd/signals.h"
#include "ardour/types.h"

#include "canvas/arrow.h"
#include "canvas/line.h"
#include "canvas/types.h"

class Editor;

class EditorCursor
{
public:
	EditorCursor (Editor&, bool (Editor::*)(GdkEvent*,ArdourCanvas::Item*));
	EditorCursor (Editor&);
	~EditorCursor ();

	void set_position (samplepos_t);
	void set_position (Temporal::timepos_t const &);


	void show ();
	void hide ();
	void set_color (Gtkmm2ext::Color);
	void set_sensitive (bool);

	Temporal::timepos_t current_time () const { return _current_time; }
	samplepos_t current_sample () const;

	ArdourCanvas::Arrow& track_canvas_item () {
		return *_track_canvas_item;
	}

	PBD::Signal1<void, Temporal::timepos_t> PositionChanged;

private:
	Editor&               _editor;
	ArdourCanvas::Arrow*  _track_canvas_item;
	Temporal::timepos_t   _current_time;
};

#endif // __gtk_ardour_editor_cursors_h__
