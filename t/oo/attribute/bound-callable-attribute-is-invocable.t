use v6;
use Test;

# A `&`-attribute bound with `:=` holds the callable in a shared cell; calling
# it (`&!v()`, `&!v.()`) must call what the cell holds. Found with
# Test::Describe::Match::Change (`&!value := sub () is raw { ... }`).
# Identical under raku.

plan 6;

class B {
    has &.v;
    submethod BUILD(:$value) { &!v := sub () { $value } }
    method t { &!v() }
    method u { &!v.() }
}
class Cc {
    has &.v;
    submethod BUILD(:$value) { &!v := -> { $value } }
    method t { &!v() }
}
my $b = B.new(value => 5);
is $b.t, 5, '&!v() after := sub';
is $b.u, 5, '&!v.() after := sub';
is $b.v.(), 5, 'the accessor result is callable';
is Cc.new(value => 6).t, 6, '&!v() after := pointy block';
is-deeply [$b.t, $b.t], [5, 5], 'repeatable';
is B.new(value => 7).t, 7, 'a second instance';
