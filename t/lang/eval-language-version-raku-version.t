use Test;

# `$*RAKU.version` is the language version of the unit compiled last: an
# `EVAL` with its own `use v6.x` reports that revision, inside the EVAL and
# after it, and an EVAL without one compiles at the calling unit's (#18867).

plan 6;

my $outer = $*RAKU.version;
is ~$outer, '6.d', 'the unit runs at 6.d';
is ~EVAL(q[use v6.c; $*RAKU.version]), '6.c', 'an EVAL with use v6.c reads v6.c';
is ~$*RAKU.version, '6.c', 'the process-wide $*RAKU keeps the last compile\'s revision';
is ~EVAL(q[$*RAKU.version]), '6.d', 'an EVAL without a pragma compiles at the caller\'s revision';
is ~$*PERL.version, '6.d', '$*PERL follows as well';
ok EVAL(q[use v6.c; $*RAKU]) === $*RAKU, '$*RAKU is one object';
