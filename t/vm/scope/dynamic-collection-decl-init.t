use Test;

# From the Test::META distribution (t/020-internals.t): the fresh empty
# `my @*x` / `my %*x` is the binding a callee sees while its own initializer
# runs, and it does not outlive the block.
plan 6;

sub arr  { (@*D // 'unbound').raku }
sub hsh  { (%*H // 'unbound').raku }

{ my @*D = <a b>; is arr(), '["a", "b"]', 'declared @*D visible'; }
is arr(), '"unbound"', '@*D is gone after the block';

{ my %*H = a => 1; is hsh(), '{:a(1)}', 'declared %*H visible'; }
is hsh(), '"unbound"', '%*H is gone after the block';

my @seen;
{ my @*E = (@seen.push((@*E // 'unbound').raku), 3); }
is @seen[0], '[]', 'initializer reads empty @*E';
{ my %*G = (a => (%*G // 'unbound').raku); is %*G<a>, '{}', 'initializer reads empty %*G'; }
