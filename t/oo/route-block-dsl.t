use v6;
use lib 't/lib';
use RouteBlockDsl;
use Test;

plan 4;

# A bare `multi name(...) is export` inside a `module ... { }` block must be
# known to the importer's parser as a callable, so the listop-with-block form
# parses as a call (Cro::HTTP::Router's `route { ... }`). Previously only
# `sub`/`multi sub` forms were discovered, and `dsl-run { ... }` parsed as a
# bareword followed by an orphan block.
my $ran = False;
my $result = dsl-run { $ran = True };
ok $ran, 'block argument of an imported bare-multi DSL sub runs';

# The module-body `our $plugin` must be visible from a method of a class
# declared in the same module body, including after the module scope has
# exited (resolved through the enclosing package chain, and not shadowed by
# a stale Nil flushed at module-load frame exit).
is $result, 'dsl:anon:got:link:cfg',
    'method of nested class reads module-body our var';

is dsl-run(:name<n1>, { 1 }), 'dsl:n1:got:link:cfg',
    'named arg still binds alongside the block';

# Direct method call from the importer, after load.
is RouteBlockDsl::Runner.new.go, 'got:link:cfg',
    'module our var readable from method called by importer';
