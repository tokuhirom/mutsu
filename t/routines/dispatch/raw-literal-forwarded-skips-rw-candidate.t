use Test;

# PDF::COS::Coercer: `method coerce($a is raw, $b is raw) { self.coerce-to($a, $b) }`
# called with a literal. The raw parameter holds the value itself, so the
# forwarded `$a` supplies no container and a multi candidate with `is rw`
# must not match (mutsu picked it and died assigning to an immutable value).

plan 6;

class C {
    multi method m(Str $o is rw, Int $t) { "rw" }
    multi method m($o, Int $t) { "plain" }
}
my $c = C.new;
sub f($x is raw) { $c.m($x, 1) }
my $v = "x";
is f("lit"), 'plain', 'a literal forwarded through an is raw parameter skips the is rw candidate';
is f($v), 'rw', 'a variable forwarded through an is raw parameter still matches it';

multi s(Str $o is rw) { "rw" }
multi s($o) { "plain" }
sub g($x is raw) { s($x) }
is g("lit"), 'plain', 'the same for a multi sub';
is g($v), 'rw', 'and a variable still matches';

class D {
    method coerce($a is raw, $b is raw) { self.coerce-to($a, $b) }
    multi method coerce-to(Str $o is rw, Int $t) { $o = "changed"; "rw" }
    multi method coerce-to($o, Int $t) { "plain" }
}
is D.new.coerce("lit", 1), 'plain', 'a literal through a raw forwarding method';
D.new.coerce($v, 1);
is $v, 'changed', 'a variable through it is written back';
