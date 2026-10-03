# No `unit module`: like P5reverse, the family lives in GLOBAL, so its
# code value is resolved by its bare name. A proto/multi family exported
# under a core routine's name: the importer's `&reverse` is this family,
# not the core routine.
proto sub reverse(|) is export {*}
multi sub reverse(List:D $l --> List:D) { $l.reverse.List }
multi sub reverse(Str() $s --> Str:D)   { $s.flip }

proto sub sort(|) is export {*}
multi sub sort(Str $s) { "user-sort:$s" }
