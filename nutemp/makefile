
AD = ..
WARNFLAGS = -Wnon-virtual-dtor -Woverloaded-virtual -fstrict-overflow -Wall -Wcast-align -Wextra -Wwrite-strings -Wunsafe-loop-optimizations -Wlogical-op -fshow-column -DARCH_X86 -mmmx -msse -mfpmath=sse -DUSE_XMMINTRIN -Wall -Wpointer-arith -Wcast-qual -Wcast-align -Wno-unused-parameter -DBOOST_SYSTEM_NO_DEPRECATED -D_ISOC9X_SOURCE -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -std=c++11 -DBOOST_NO_AUTO_PTR -Woverloaded-virtual -Wno-unused-local-typedef,
CXXFLAGS = -g $(WARNFLAGS)
CPPFLAGS = -I $(AD)/libs/ardour -I $(AD)/libs/temporal -I $(AD)/libs/pbd -I $(AD)/build/libs/pbd -I $(AD)/libs/timecode
CPPFLAGS += `pkg-config --cflags glibmm-2.4` `pkg-config --cflags libxml-2.0`
LDFLAGS = -L $(AD)/build/libs/temporal -ltemporal -Xlinker -rpath=$(AD)/build/libs/temporal
LDFLAGS += -L $(AD)/build/libs/pbd -lpbd -Xlinker -rpath=$(AD)/build/libs/pbd
LDFLAGS += -L $(AD)/build/libs/temporal -ltemporal -Xlinker -rpath=$(AD)/build/libs/temporal
LDFLAGS += `pkg-config --cflags --libs glibmm-2.4` `pkg-config --cflags --libs libxml-2.0`

all: t

t.o: t.cc t.h

t: t.o
	$(CXX) $(LDFLAGS) -o $@ $?

clean:
	rm -f t t.o
