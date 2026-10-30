use Test;

# Named arguments to a plain method are keyed by name and value type for the
# method lane, and a named container argument still shares its container.
plan 8;

class C {
    method m(Int $x, Bool :$flag = True --> Int) { $flag ?? $x !! -$x }
    method push-into(:$n) { $n.push(7); $n.elems }
}

my $c = C.new;
my $t = True;
my $f = False;

for ^3 {
    is $c.m(1, :flag($t)), 1, "named True, call $_";
}
is $c.m(2, :flag($f)), -2, "named False picks the other value";
is $c.m(3), 3, "named omitted uses the default";
is $c.m(4, flag => $f), -4, "pair syntax";

my @a = 1, 2;
is $c.push-into(n => @a), 3, "named array shares its container";
is @a.elems, 3, "the caller's array grew";
