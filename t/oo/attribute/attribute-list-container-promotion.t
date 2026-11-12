use Test;

# `($!x, $!y)` holds the attributes' own containers: writing through the list
# writes the attribute, and a typed attribute's constraint travels with its
# container, on the first call that promotes it and on every later one (which
# reuses the promoted cell). Red's `method args { $!left, $!right }` builds
# such a list for every AST node it compares.

plan 9;

class C {
    has Int $!x = 1;
    has $.y = 2;
    has $!z;
    method c { ($!x, $!y, $!z) }
    method x { $!x }
}
my $c = C.new;
for 1..2 -> $round {
    my @l := $c.c;
    @l[0] = 5;
    is $c.x, 5, "round $round: a write through the list reaches the private attribute";
    throws-like { @l[0] = "s" }, X::TypeCheck::Assignment,
        "round $round: the attribute's type constraint travels with its container";
    @l[1] = 7;
    is $c.y, 7, "round $round: a public attribute too";
    @l[2] = 9;
    is $c.c[2], 9, "round $round: an untyped private attribute too";
}
is C.new.c[0], 1, 'a fresh object starts with its own containers';
