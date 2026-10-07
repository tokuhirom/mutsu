use v6;
use Test;

# From HTML::Tag: a role method recursing into an instance of ANOTHER class
# composing the same role with a different argument must not clobber the
# caller's own `$T` when it returns.

plan 3;

role G[$T] {
    has $.c;
    method f() { my $before = $T; $!c.f if $!c.defined; "$before/$T" }
    method render() {
        my $s = "<$T>";
        $s ~= $!c.render if $!c.defined;
        $s ~ "</$T>";
    }
}
class P does G['p'] {}
class A does G['a'] {}

is P.new(c => A.new).f, 'p/p', 'caller keeps its own role parameter';
is P.new(c => A.new).render, '<p><a></a></p>', 'closing tag uses own parameter';
is P.new(c => P.new(c => A.new)).render, '<p><p><a></a></p></p>', 'nested three deep';

# vim: expandtab shiftwidth=4
