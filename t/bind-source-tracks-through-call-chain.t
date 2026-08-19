use Test;
plan 8;

# Pin for todo/deep/attr-bind-source-write-lost-through-nested-sub-call-chain.md:
# a `$alias := $var` bind of a free/outer lexical performed inside a named sub
# (or a closure, or a method) must leave $alias tracking every LATER write to
# $var, no matter how deep the call chain that performed the bind was.

# 1: direct named-sub call (SetGlobal bind path, ADR-0024 mainline cell reuse)
{
    my $var = 100;
    my $alias;
    sub bindit { $alias := $var }
    bindit();
    is $alias, 100, 'direct sub call: alias reads bound value';
    $var = 200;
    is $alias, 200, 'direct sub call: alias tracks source writes';
}

# 2: bind through a wrapper sub taking a &-param (multi-frame chain)
{
    my $var = 100;
    my $alias;
    sub bindit2 { $alias := $var }
    sub wrap2(&c) { c() }
    wrap2({ bindit2() });
    $var = 200;
    is $alias, 200, 'wrapper-sub chain: alias tracks source writes';
}

# 3: bind through a wrapper with try + trailing call (lives-ok shape)
{
    my $var = 100;
    my $alias;
    sub bindit3 { $alias := $var }
    sub wrap3(&c) { try { c(); }; 'x'.chars; }
    wrap3({ bindit3() });
    $var = 200;
    is $alias, 200, 'try-wrapper chain: alias tracks source writes';
}

# 4: writes made from inside another sub also reach the alias
{
    my $var = 100;
    my $alias;
    sub bindit4 { $alias := $var }
    sub writevar4 { $var = 300 }
    bindit4();
    writevar4();
    is $alias, 300, 'sub-side source write reaches the alias';
}

# 5: in-frame tracking right after the bind, inside the binding sub itself
{
    my $var = 100;
    my $seen;
    sub bindit5 { my $a := $var; $var = 150; $seen = $a; }
    bindit5();
    is $seen, 150, 'bind tracks a source write within the same frame';
}

# 6: attribute bind ($!x := $var) through a lives-ok-style chain
{
    my $var = 100;
    my class K6 { has $.x; method bind { $!x := $var } }
    my $obj = K6.new;
    sub wrap6(&c) { try { c(); }; 'x'.chars; }
    wrap6({ $obj.bind() });
    is $obj.x, 100, 'attr bind through chain: reads bound value';
    $var = 200;
    is $obj.x, 200, 'attr bind through chain: tracks source writes';
}
