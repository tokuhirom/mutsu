unit module ProtoRecursionFixture;

# A `proto`/`multi` pair under a name that does NOT collide with any mutsu
# builtin/listop (unlike e.g. `head`), so the exported name only ever
# resolves through the ordinary user-registry/env path this fixture exists
# to exercise -- see t/routine-value-self-recursion-after-import-scope-pop.t.
proto sub xrecur($?) is export {*}
multi sub xrecur(Int $n) { "xrecur-int($n)" }
multi sub xrecur(Str $s) { "xrecur-str($s)" }
