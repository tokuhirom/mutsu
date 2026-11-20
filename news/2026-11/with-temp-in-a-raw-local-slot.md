# A `with`'s condition is held in a local slot, not by name

`with X { BODY }` (and `without`, `orwith`) desugars to
`if (my $__with_tmp_N = X).defined { given $__with_tmp_N { BODY } }`. The
temp holds the once-evaluated condition. It was kept off a local slot because
a slot store itemizes: `with (1,2,3) -> @m` must bind the List itself. So
every `with` paid by-name costs:

- a declaration bookkeeping step;
- a store into the `our`-variable table;
- two lookups that walk the name resolution chain.

The temp now takes a local slot, written by a new `SetLocalRaw` opcode. The
store keeps the value exactly as given: no Scalar itemization, no Proxy
FETCH, and no readonly, type or env-mirror bookkeeping a user variable needs.
The topicalizing `given` reads the slot back raw. An rw call's deferred
element token (`without slot(4) { $_ = 5 }`) therefore still reaches the
topic, and the body's write lands in the element.

Callgrind on a profiling build, Ir per call of a small sub including the loop
around it:

| sub body | before | after |
| --- | ---: | ---: |
| `with $s.index('/', 1) { $x = $_ }` | 32.2k | 20.4k |
| `without $s.index('<', 1) { $x = 1 }` | 41.4k | 29.6k |
| reference: `given $s.index('/', 1) { $x = $_ }` | 17.6k | 17.7k |

`with` now costs 1.16x `given`, inside #18208's 1.2x goal.
