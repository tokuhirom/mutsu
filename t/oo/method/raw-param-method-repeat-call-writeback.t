use Test;

# An `is raw` / `is rw` method parameter binds the caller's container on
# every call, not only the first: the cached fast method dispatch must not
# take a method with such a parameter (#13207, PDF's
# `self!coercer.coerce($a, $b)` relaying an `is raw` `$a`).

plan 4;

class Inner { method coerce($a is raw) { $a = "<$a>" } }
class Outer {
    method coerce($a is raw) { self.inner.coerce($a) }
    method inner { Inner }
}

my $y = 'y';
Outer.coerce($y);
is $y, '<y>', 'first call writes back';
my $x = 'x';
Outer.coerce($x);
is $x, '<x>', 'second call writes back too';
Outer.coerce($x);
is $x, '<<x>>', 'and the third';

my %h = a => 'v';
my $c := %h.AT-KEY('a');
Outer.coerce($c);
is %h<a>, '<v>', 'a bound element is written through a repeated call';
