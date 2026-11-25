# A survey of why mutsu trails Raku++ on 18 bench rows

`docs/rakupp-gap-survey.md` takes every bench row where mutsu is behind Raku++
(`ratio_mutsu_over_rakupp` > 1 at `13320ba6`) and records, with callgrind,
cachegrind and rusage measurements of both interpreters, what each gap is made
of and which open issue already covers each part.

Apart from the JSON::Fast rows, where rakupp substitutes a native parser, three
costs are shared by every row. Startup is ~3 ms slower in kernel time: mutsu
touches ~28 MB of its own text to build the interpreter, so it takes 4x the
page faults at the same instruction count. Equal instruction counts run slower,
with about twice the I-cache and D-cache misses on the regex rows. glibc malloc is a
10-20% share, and preloading jemalloc makes those rows 5-15% faster. The first
two are tracked only partly (#15737, #17504), and the allocator result
complements the negative mimalloc note in #15235. The survey lists them as
uncovered so issues can be filed for them.
