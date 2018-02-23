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

#include "temporal/range.h"

using namespace Temporal;

/** Subtract the ranges in `sub' from this range returning the result.
 */
RangeList
Range::subtract (RangeList & sub) const
{
	/* Start with the input range */

	RangeList result;

	result.add (*this);

	if (sub.empty () || empty()) {
		return result;
	}

	RangeList::List s = sub.get ();

	/* The basic idea here is to keep a list of the result ranges, and subtract
	   the bits of `sub' from them one by one.
	*/

	for (typename RangeList::List::const_iterator i = s.begin(); i != s.end(); ++i) {

		/* Here's where we'll put the new current result after subtracting *i from it */
		RangeList new_result;

		typename RangeList::List r = result.get ();

		/* Work on all parts of the current result using this range *i */
		for (typename RangeList::List::const_iterator j = r.begin(); j != r.end(); ++j) {

			switch (Temporal::coverage (j->from, j->to, i->from, i->to)) {
			case OverlapNone:
				/* The thing we're subtracting (*i) does not overlap this bit of the result (*j),
				   so pass it through.
				*/
				new_result.add (*j);
				break;
			case OverlapInternal:
				/* Internal overlap of the thing we're subtracting (*i) from this bit of the result,
				   so we should end up with two bits of (*j) left over, from the start of (*j) to
				   the start of (*i), and from the end of (*i) to the end of (*j).
				*/
				assert (j->from < i->from);
				assert (j->to > i->to);
				new_result.add (Range (j->from, i->from.decrement()));
				new_result.add (Range (i->to + 1, j->to));
				break;
			case OverlapStart:
				/* The bit we're subtracting (*i) overlaps the start of the bit of the result (*j),
				 * so we keep only the part of of (*j) from after the end of (*i)
				 */
				assert (i->to < j->to);
				new_result.add (Range (i->to + 1, j->to));
				break;
			case OverlapEnd:
				/* The bit we're subtracting (*i) overlaps the end of the bit of the result (*j),
				 * so we keep only the part of of (*j) from before the start of (*i)
				 */
				assert (j->from < i->from);
				new_result.add (Range (j->from, i->from.decrement ()));
				break;
			case OverlapExternal:
				/* total overlap of the bit we're subtracting with the result bit, so the
				   result bit is completely removed; do nothing */
				break;
			}
		}

		new_result.coalesce ();
		result = new_result;
	}

	return result;
}

samplepos_t
Range::start_sample () const
{
	assert (from.lock_style() == Temporal::AudioTime);
	return from.sample ();
}

samplepos_t
Range::end_sample () const
{
	assert (from.lock_style() == Temporal::AudioTime);
	return to.sample ();
}

samplecnt_t
Range::length_samples () const
{
	return end_sample() - start_sample();
}
