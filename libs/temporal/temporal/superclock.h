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

#ifndef __temporal_superclock_h__
#define __temporal_superclock_h__

#include <stdint.h>

namespace Temporal {

/* should really be unsigned but creates all sort of silly signed/unsigned math
 * issues when used with position and offset values that can be positive or
 * negative.
 */
typedef int64_t superclock_t;

static const superclock_t superclock_ticks_per_second = 508032000; // 2^10 * 3^4 * 5^3 * 7^2

static inline superclock_t superclock_to_samples (superclock_t s, int sr) { return (s * sr) / superclock_ticks_per_second; }
static inline superclock_t samples_to_superclock (int samples, int sr) { return (samples * superclock_ticks_per_second) / sr; }

}

#endif /* __temporal_superclock_h__ */
