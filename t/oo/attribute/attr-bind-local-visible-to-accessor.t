use Test;

# `$!r := $local` inside a method makes the attribute that container at once:
# an accessor called on `self` later in the same method must see it. mutsu kept
# the bound container only in the method's attribute slot until the method
# returned, so `self.r` / `$.r` read the old value in between (#9498).

plan 7;

class H {
    has $.r;
    method bind-and-read {
        my $x = 42;
        $!r := $x;
        my @seen = self.r, $.r;
        $x = 43;
        @seen.push: self.r;
        @seen
    }
    method bind-type {
        my $t := Metamodel::ClassHOW.new_type(:name("X::Y"));
        $t.^compose;
        $!r := $t;
        (self.r.^name, $.r.^name)
    }
}

my $h = H.new;
my @seen = $h.bind-and-read;
is @seen[0], 42, 'self.r sees the bound container in the same method';
is @seen[1], 42, '$.r sees it too';
is @seen[2], 43, 'and keeps aliasing it after the local changes';
is $h.r, 43, 'the binding survives the method';

my ($via-self, $via-dot) = H.new.bind-type;
is $via-self, 'X::Y', 'a bound run-time type object reads back through self.r';
is $via-dot, 'X::Y', 'and through $.r';

class H2 { has $.r; method go { my $x = 1; $!r := $x; $x = 2; self.r } }
is H2.new.go, 2, 'a write to the bound local is visible through the accessor';
