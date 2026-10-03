use v6;
use Test;

plan 12;

# An outer variable a class/role METHOD assigns belongs to the scope the class
# was declared in. A readonly (non-`is rw`) parameter of whoever calls the
# method must not make that assignment fail just because the two share a name
# (#11054; found via P5print, whose test's `FakeHandle.printf` assigns the
# file-scope `my $format` while the module's `printf` sub calling it has its
# own readonly `$format` parameter).

my $v = 0;
class FH {
    method m($t) { $v = $t }
    method !p($t) { $v = $t }
    method via-private($t) { self!p($t) }
    multi method mm(Str $t) { $v = $t }
    multi method mm(Int $t) { $v = $t * 2 }
}

sub call-m($v) { FH.m($v) }
call-m('y');
is $v, 'y', 'method assigns an outer variable named like the caller\'s readonly param';

sub call-private($v) { FH.via-private($v) }
call-private('p');
is $v, 'p', 'private method assigns the outer variable';

sub call-multi($v) { FH.mm($v) }
call-multi('s');
is $v, 's', 'multi method (Str candidate) assigns the outer variable';
call-multi(21);
is $v, 42, 'multi method (Int candidate) assigns the outer variable';

sub call-instance($v) { FH.new.m($v) }
call-instance('i');
is $v, 'i', 'method called on an instance assigns the outer variable';

my $r = 0;
role R { method set($x) { $r = $x } }
class WithRole does R { }
sub call-role($r) { WithRole.set($r) }
call-role('role');
is $r, 'role', 'composed role method assigns the outer variable';

# The caller's own parameter is still readonly once the method returns.
sub still-readonly($v) {
    FH.m('x');
    try { $v = 1 };
    $!.defined;
}
ok still-readonly('a'), 'caller\'s readonly parameter is restored after the call';

# A class declared inside a routine: the method writes that routine's lexical.
sub make-inner($w) {
    my $inner = 0;
    my class Inner { method set($x) { $inner = $x } }
    Inner.set($w);
    $inner;
}
sub call-inner($inner) { make-inner($inner) }
is call-inner('in'), 'in', 'method of a class declared in a routine assigns that routine\'s lexical';

# A readonly binding in the declaring scope stays readonly inside the method.
for 1 -> $alias {
    my class InLoop { method set { $alias = 5 } }
    throws-like { InLoop.set }, Exception,
        'method still refuses to assign a readonly loop alias of its declaring scope';
}

my $imm := 42;
class Imm { method set { $imm = 1 } }
sub call-imm($x) { Imm.set }
throws-like { call-imm(1) }, Exception, message => /immutable/,
    'method still refuses to assign an outer variable bound to an immutable value';

my $late-imm;
class Late { method set { $late-imm = 1 } }
$late-imm := 7;
throws-like { Late.set }, Exception, message => /immutable/,
    'an immutable bind made after the class was declared is still honored';

# The method's write reaches the immutable binding even when the caller's
# readonly parameter shares its name (#11142, ADR-11142).
sub shadow-imm($imm) { Imm.set }
throws-like { shadow-imm(1) }, Exception,
    message => 'Cannot assign to an immutable value',
    'method refuses an immutable outer variable named like the caller\'s readonly param';
