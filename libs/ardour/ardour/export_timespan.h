/*
    Copyright (C) 2008 Paul Davis
    Author: Sakari Bergen

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

#ifndef __ardour_export_timespan_h__
#define __ardour_export_timespan_h__

#include <string>

#include <boost/shared_ptr.hpp>

#include "ardour/libardour_visibility.h"
#include "ardour/types.h"

namespace ARDOUR
{

class ExportStatus;
class ExportChannel;
class ExportTempFile;

class LIBARDOUR_API ExportTimespan
{
  private:
	typedef boost::shared_ptr<ExportStatus> ExportStatusPtr;

  private:
	friend class ExportElementFactory;
	ExportTimespan (ExportStatusPtr status, samplecnt_t sample_rate);

  public:
	~ExportTimespan ();

	std::string name () const { return _name; }
	void set_name (std::string name) { _name = name; }

	std::string range_id () const { return _range_id; }
	void set_range_id (std::string range_id) { _range_id = range_id; }

	bool realtime () const { return _realtime; }
	void set_realtime (bool rt) { _realtime = rt; }

	void set_range (Temporal::timepos_t const & start, Temporal::timepos_t const & end);
	samplecnt_t get_length () const { return end.sample() - start.sample(); }
	samplepos_t get_start () const { return start.sample(); }
	samplepos_t get_end () const { return end.sample(); }

	/// Primarily compare start time, then end time
	bool operator< (ExportTimespan const & other) {
		if (start  < other.start) { return true; }
		if (start > other.start) { return false; }
		return end < other.end;
	}

  private:

	ExportStatusPtr status;

	Temporal::timepos_t start;
	Temporal::timepos_t end;
	Temporal::timepos_t position;

	samplecnt_t         sample_rate;

	std::string _name;
	std::string _range_id;
	bool        _realtime;

};

} // namespace ARDOUR

#endif /* __ardour_export_timespan_h__ */
