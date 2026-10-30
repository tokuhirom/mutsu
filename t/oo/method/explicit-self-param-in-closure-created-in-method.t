use Test;

# ADR-14843: a closure created inside a method carries the method's frame
# record, but a plain routine's own `$self` parameter (`sub ($self) { ... }`,
# `-> $self { ... }`) names the argument it was bound to, never the creator's
# invocant. From Cro::HTTP::Cookie's RFC 1123 formatter. Expected values are
# raku's.

plan 4;

class C {
    has $.d = DateTime.new(2020, 1, 2, 3, 4, 5);
    sub fmt(DateTime $_) {
        my $f = sub ($self) { sprintf "%04d", .year given $self.utc };
        $f($_)
    }
    method m { fmt($!d) }
}
is C.new.m, '2020', 'a formatter sub with a $self parameter reads its argument';

class D {
    method pointy { my $h = -> $self { $self ~ "!" }; $h("z") }
    method second { my $f = sub ($x, $self) { $x ~ $self }; $f("a", "b") }
    method plain-self { my $f = sub { self.^name }; $f() }
}
is D.new.pointy, 'z!', 'a pointy block with a $self parameter';
is D.new.second, 'ab', '$self in a later position';
is D.new.plain-self, 'D', 'a closure without the parameter still sees the method self';
