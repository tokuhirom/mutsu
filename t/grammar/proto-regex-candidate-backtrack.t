# A `regex` caller backtracks into the `regex` candidate a proto dispatched to,
# but never on to the proto's other candidates. Found via
# Lingua::NumericWordForms, whose German grammar separates `hundert-vier` with
# `regex preceding-number-separator:sym<German> { \h* | <:Pd> | ... }`: the
# empty `\h*` branch is tried first, and the `-` only by backtracking.
use Test;

plan 8;

grammar Greedy { regex TOP { <num> '9' }; proto token num {*}; regex num:sym<d> { \d* } }
ok Greedy.parse('129'), 'a greedy regex candidate gives back characters';

grammar Sep {
    regex TOP { 'h' <sep> 'v' }
    proto token sep {*}
    regex sep:sym<x> { \h* | <:Pd> }
}
ok Sep.parse('h-v'), 'a later | branch of the candidate is reached by backtracking';
ok Sep.parse('hv'), 'and the empty branch still matches';
is ~Sep.parse('h-v')<sep>, '-', 'the candidate Match is the backtracked one';

grammar Ratchet { token TOP { <num> '9' }; proto token num {*}; regex num:sym<d> { \d* } }
nok Ratchet.parse('129'), 'a ratcheted caller keeps the first end';

grammar TokenCand { regex TOP { <num> '9' }; proto token num {*}; token num:sym<d> { \d* } }
nok TokenCand.parse('129'), 'a token candidate does not give back';

grammar NextCand {
    regex TOP { <sep> 'b' }
    proto regex sep {*}
    token sep:sym<long> { 'aab' }
    token sep:sym<short> { 'aa' }
}
nok NextCand.parse('aab'), 'a failure after the candidate returned does not try the next candidate';

grammar Words {
    regex TOP { <h> [ <.sep>? <u> ]? }
    token h { 'hundert' }
    token u { 'vier' }
    proto token sep {*}
    regex sep:sym<General> { \h+ 'and' \h+ | \h+ }
    regex sep:sym<German> { \h* | <:Pd> | \h* <:Pd>? 'und' <:Pd>? \h* }
}
is ~Words.parse('hundert-vier')<u>, 'vier', 'the Lingua::NumericWordForms shape';
