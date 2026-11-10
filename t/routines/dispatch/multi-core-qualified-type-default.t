use Test;

# A multi candidate whose parameter has a defaulted `CORE::`-qualified core
# type is applicable to a call that omits it.
plan 6;

multi sub pinned(CORE::Int $n = 5) { "pinned $n" }
multi sub revised(CORE::v6c::Int:D $n = 6) { "revised $n" }
multi sub stash(CORE::v6c::PseudoStash:D $s = CORE::) { "stash " ~ $s.^name }
multi sub two(CORE::v6c::Int $n = 7) { "int" }
multi sub two(Str $s) { "str" }

is pinned(), 'pinned 5', 'CORE::Int with a default';
is revised(), 'revised 6', 'CORE::v6c::Int:D with a default';
is stash(), 'stash PseudoStash', 'a PseudoStash default';
is two(), 'int', 'with a competing candidate';
is two("a"), 'str', 'the other candidate still wins on a Str';
is revised(9), 'revised 9', 'an argument replaces the default';
