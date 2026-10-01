use v6;
use Test;

plan 6;

# A `use lib` nested in a routine or block is a BEGIN-time effect: it extends
# the process-wide repository chain while the unit is compiled, whether or not
# its scope ever runs (#10481). It used to take effect only when the scope ran.

sub chain-has($frag) { $*REPO.repo-chain.map(*.Str).grep(*.contains($frag)).elems }

sub never-called { use lib "/nonexist-nlb-sub" }
is chain-has("nlb-sub"), 1, 'use lib in a routine that is never called';

sub never-called-block { { use lib "/nonexist-nlb-block" } }
is chain-has("nlb-block"), 1, 'use lib in a block of a routine that is never called';

sub twice { use lib "/nonexist-nlb-twice" }
twice();
twice();
is chain-has("nlb-twice"), 1, 'running the routine does not add the path again';

my $seen;
sub begin-behind { use lib "/nonexist-nlb-begin"; BEGIN { $seen = chain-has("nlb-begin") } }
is $seen, 1, 'a BEGIN behind a nested use lib sees its path';

sub order { use lib "/nonexist-nlb-first"; use lib "/nonexist-nlb-second" }
my @chain = $*REPO.repo-chain.map(*.Str);
ok @chain.first(*.contains("nlb-first"), :k) > @chain.first(*.contains("nlb-second"), :k),
    'nested use lib paths are prepended in source order';

sub loads { use lib "t/fixtures/lib-precedence/plain" }
use PrecProbe;
is prec-probe-who(), 'plain', 'a later top-level use resolves through a nested use lib';
