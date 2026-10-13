use v6.d;
use Test;

# From the TOML distribution (lib/TOML/NQP.rakumod): a table path is walked by
# rebinding a dynamic variable one key at a time, `$*tml := $*tml{$key}`, from a
# routine that did not declare `$*tml`, and every `key = value` line lands via
# `my $*ptr := $*tml; $*ptr := $*ptr{$k}; $*ptr = $v`.

plan 8;

# A `my`-declared dynamic that shares its cell with another variable and is then
# rebound to a missing element must detach from that cell, not clobber it.
{
    my $h = {};
    {
        my $*p := $h;
        $*p := $*p<a>;
        $*p = 5;
    }
    is-deeply $h, {a => 5}, 'rebinding a bound dynamic to a missing element leaves the source alone';
}

{
    my $h = {};
    for <a b c> -> $k {
        my $*ptr := $h;
        $*ptr := $*ptr{$k};
        $*ptr = 5;
    }
    is-deeply $h, {a => 5, b => 5, c => 5}, 'each loop iteration gets a fresh dynamic binding';
}

# A callee walking the caller's dynamic through several keys.
sub walk-for(@parts) {
    $*tml := $*tld;
    for 0 ..^ @parts { $*tml := $*tml{@parts[$_]}; }
    $*tml //= {};
    True
}
sub walk-while {
    $*tml := $*tld;
    my $i = 0;
    while $i < 3 { $*tml := $*tml{<a b c>[$i]}; $i++ }
    $*tml //= {};
    True
}
sub walk-ternary(@parts) {
    $*tml := $*tld;
    for 0 ..^ @parts {
        $*tml := $*tml ~~ List ?? $*tml[*-1]{@parts[$_]} !! $*tml{@parts[$_]};
    }
    $*tml //= {};
    True
}
sub run-walk(&walk, |c) {
    my $*tld = {};
    my $*tml = $*tld;
    walk(|c);
    $*tld
}

is-deeply run-walk(&walk-for, <a b c>.List.Array), {a => {b => {c => {}}}},
    'a for loop walks three keys of the caller dynamic';
is-deeply run-walk(&walk-while), {a => {b => {c => {}}}},
    'a while loop re-executing the same rebind keeps advancing';
is-deeply run-walk(&walk-ternary, <a b>.List.Array), {a => {b => {}}},
    'a rebind through a ternary over elements autovivifies the selected branch';

# The ternary form on a plain lexical.
{
    my $t = {};
    my $m = $t;
    $m := $m ~~ List ?? 1 !! $m<g>;
    $m //= {};
    is-deeply $t, {g => {}}, 'lexical rebind through ?? !! over a missing element';
}

# `chr` as a function numifies a Str like `Str.chr` does.
is chr(":16<41>"), 'A', 'chr() of a radix literal string';
is ":16<e9>".chr, chr(":16<e9>"), 'function and method forms agree';

done-testing;
