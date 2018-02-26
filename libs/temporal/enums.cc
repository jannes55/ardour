/*
    Copyright (C) 2018 Paul Davis

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
#include <typeinfo>
#include <vector>

#include "pbd/enumwriter.h"
#include "temporal/types.h"
#include "temporal/tempo.h"

using namespace PBD;
using namespace Temporal;

void
Temporal::setup_enum_writer ()
{
	EnumWriter& enum_writer (EnumWriter::instance());
	std::vector<int> i;
	std::vector<std::string> s;

	TimeDomain _TimeDomain;
	Tempo::Type _Tempo_Type;

#define REGISTER(e) enum_writer.register_distinct (typeid(e).name(), i, s); i.clear(); s.clear()
#define REGISTER_BITS(e) enum_writer.register_bits (typeid(e).name(), i, s); i.clear(); s.clear()
#define REGISTER_ENUM(e) i.push_back (e); s.push_back (#e)
#define REGISTER_CLASS_ENUM(t,e) i.push_back (t::e); s.push_back (#e)

	REGISTER_ENUM (AudioTime);
	REGISTER_ENUM (BeatTime);
	REGISTER_ENUM (BarTime);
	REGISTER (_TimeDomain);

	REGISTER_CLASS_ENUM(Tempo, Ramped);
	REGISTER_CLASS_ENUM(Tempo, Constant);
	REGISTER (_Tempo_Type);
}
