# An unrelated `:=` no longer changes what a scalar store costs

The scalar-store fast path checks whether the slot it writes has a
`__mutsu_sigilless_alias::` key by testing that key's bit in a per-key bitset.
Before #10765 it probed the env instead. Until now, a whole-program "any alias
key yet" latch sat in front of the bit test. The latch made a program with no
bind slightly cheaper. It also made the first unrelated bind anywhere add the
whole per-slot check, about 15 instructions, to every scalar store in the
process.

The latch is gone. The bit test also answers a program that never binds, since
every bit is clear there. The test now reads the slot's `alias_sym` from the
`BindingDesc` that the fast-path gate already fetched for its
simple-scalar flag, so the descriptor is looked up once, and the check is
inlined.

On the #8748 repro pair with the JIT on (callgrind, profiling build), adding
`my @unused := @data` cost +2,107,233 instructions (+0.57%). It now costs
+311,528 (+0.08%), which is the bind statement itself: no function differs by
more than 30k instructions between the two programs. The program with no bind
pays about 6 more instructions per store than before (+0.2%). It now pays
exactly what the program with the bind pays. Closes #10691.
