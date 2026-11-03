use Test;
# From PDF::COS `coerce`: `:$x` forwards a read-only parameter, which is no
# writable lvalue, so a multi method's `is rw` candidate yields to `is copy`.
plan 6;

class K {
    multi method c(Str :$l! is rw) { "rw" }
    multi method c(Str :$l! is copy) { "copy" }
}
role R { method hi { "hi" } }
class M {
    method !add($obj is rw, $r) { $obj.does($r) ?? $obj !! $obj = $obj but $r }
    multi method co(Str :$l! is rw) { self!add($l, R) }
    multi method co(Str :$l! is copy) { self.co: :$l }
}

sub f(Str :$l!) { K.c(:$l) }
sub g(Str $l)   { K.c(:$l) }
is f(:l<a>), 'copy', 'forwarded read-only named param';
is g('a'), 'copy', 'forwarded read-only positional param';
my $v = 'a';
is K.c(:l($v)), 'rw', 'a variable still binds the is rw candidate';
is K.c(:l<a>), 'copy', 'a literal binds is copy';

sub h(Str :$l!) { M.co(:$l) }
is h(:l<abc>).hi, 'hi', 'rw candidate mixes a role into the copy';
my $w = 'abc';
is M.co(:l($w)).hi, 'hi', 'the rw candidate works on a variable';
