use Test;

# A twigil-less `has $x` bound to a plain value (`$x := 42`) has no container
# any more, as `$!x := 42` leaves it: a later `$x = 5` dies, in another
# method or the same one, and the value stays (#18365).

plan 9;

class K {
    has $x;
    method b { $x := 42 }
    method s { $x = 5 }
    method g { $x }
}
my $o = K.new;
$o.b;
is $o.g, 42, 'the bound value is read back';
throws-like { $o.s }, Exception, 'assigning it in another method dies';
is $o.g, 42, 'and the value is unchanged';

class B {
    has $x;
    method b { $x := 42; try { $x = 1 }; $! ?? 'dies' !! $x }
}
is B.new.b, 'dies', 'assigning it in the same method dies';

class C {
    has $x = 3;
    method b { my $v = 1; $x := $v; $x = 7; $v }
}
is C.new.b, 7, 'a bound variable is still written through';

class D {
    has $x;
    method b { $x := 42 }
    method r { $x := 43 }
    method g { $x }
}
my $d = D.new;
$d.b;
$d.r;
is $d.g, 43, 'a rebind replaces the bound value';

class P {
    has $!x;
    method b { $!x := 42 }
    method s { $!x = 5 }
    method g { $!x }
}
my $p = P.new;
$p.b;
throws-like { $p.s }, Exception, 'the $!x spelling dies too';
is $p.g, 42, 'and keeps its value';

class E {
    has $x;
    method b { $x := 42 }
    method g { $x.VAR.^name }
}
my $e = E.new;
$e.b;
is $e.g, 'Int', 'the attribute holds the value itself, not a Scalar';
