#include <queue>

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

struct flit {
    uint64_t data;
    uint8_t keep;
    int last;
};

std::queue<flit> flits;

extern "C" int trace_rom_init(const char *fname)
{
    FILE *f;
    flit flt;

    f = fopen(fname, "r");

    while (!feof(f)) {
        if (fscanf(f, "%lx %x %d\n", &flt.data, &flt.keep, &flt.last) != 3) {
            perror("fscanf()");
            abort();
        }
        flits.push(flt);
    }

    return flits.size();
}

extern "C" void trace_rom_tick(
        char *stream_valid,
        char stream_ready,
        long *stream_data,
        char *stream_keep,
        char *stream_last)
{
    if (!flits.empty() && stream_ready) {
        flits.pop();
    }

    if (flits.empty()) {
        *stream_valid = 0;
        *stream_data = 0;
        *stream_keep = 0;
        *stream_last = 0;
    } else {
        *stream_valid = 1;
        *stream_data = flits.front().data;
        *stream_keep = flits.front().keep;
        *stream_last = flits.front().last;
    }
}
