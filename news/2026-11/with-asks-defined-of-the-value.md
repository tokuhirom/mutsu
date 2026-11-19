# `with` and `without` ask `.defined` of the value, not of the variable

`with X { ... }` is lowered to `if (my $__with_tmp_N = X).defined { given ... }`.
The statement modifier `S without X` is lowered to `given X { if !.defined { S } }`.
In both cases `.defined` was asked of a *variable*, and a method call on a
variable compiles to a by-name `CallMethodMut`. That form exists so that a
method which replaces the variable's contents can write it back. It took the
general method path, about 21k instructions, where the same call on a value
costs about 2k.

`.defined` never writes its invocant's variable. So for the topic `$_` and for
a `with` temp's declaration, the compiler now reads the value and asks it with
a plain `CallMethod`. A Failure is still marked handled, because the mark is
kept per instance. A user class's own `defined` method still decides, and a
Junction still autothreads.

The callgrind slope on a profiling build, per call of a small sub:

| sub body | before | after |
| --- | ---: | ---: |
| `without $s.index('<', 1) { ... }` | 43.5k | 41.3k |
| `return 5 without $s.index('<', 1)` | 29.3k | 27.1k |

The equivalent `my $i = ...; if !$i.defined { ... }` is 16.6k. What remains of
the gap is mostly that the hidden `$__with_tmp_N` is a by-name variable: an
`our`-table write and two by-name reads. Keeping it off a slot avoids
itemizing the value, so moving it to a raw slot store is the next step. The
`given` that topicalizes the body (~9k) is the other part. The work is part
of #18208 and #16897.
