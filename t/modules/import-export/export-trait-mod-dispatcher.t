use lib 't/lib';
use Test;
use ExportedAttrTraitEnv;

# From ecosystem Trait::Env: a `&trait_mod:<is>` handed over by `sub EXPORT` is
# a trait handler by itself (no `use Test` needed), and dispatching through its
# captured candidates picks the candidate whose `:%env` / `List :$env` /
# `:$env` named parameter really accepts the argument, not the first one.
plan 6;

class C {
    has $.s is env;
    has $.h is env(:k(1));
    has $.l is env([1, 2]);
    has $.n is env(5);
}

is C.new.s, 's-scalar:Any', 'bare `is env` takes the scalar candidate';
is C.new.h, 'h-hash:Any', '`is env(:k(1))` takes the %env candidate';
is C.new.l, 'l-list:Any', '`is env([1,2])` takes the List candidate';
is C.new.n, 'n-scalar:Any', '`is env(5)` takes the scalar candidate';

my $v is env;
is $v, 'scalar', 'a variable trait through the exported dispatcher';
my $w is env(:a(1));
is $w, 'hash', 'and its %env candidate';
