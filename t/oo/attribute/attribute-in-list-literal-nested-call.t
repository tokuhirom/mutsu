use Test;

# An attribute in a list literal is the invocant's own attribute container.
# It used to be boxed into a private cell published by name, which a nested
# call of the same method on another invocant then read as its own `$!name`.

plan 4;

class Node {
    has $.name;
    has $.parent;
    method path(Str :$tail!) {
        my $p = ($!name, $tail).map({ $_ }).join('/');
        $!parent ?? $!parent.path(:tail($p)) !! $p
    }
}

my $leaf = Node.new(name => 'c', parent => Node.new(name => 'b', parent => Node.new(name => 'a')));
is $leaf.path(:tail('x')), 'a/b/c/x', 'each level reads its own attribute';

class Box {
    has $.v = 3;
    method alias() { my $l = ($!v, 1); $l[0] = 9; $!v }
    method copy() { my @a = ($!v, 2); @a[0] = 7; $!v }
}
my $b = Box.new;
is $b.alias, 9, 'a list element aliases the attribute';
is $b.copy, 9, 'an array assignment copies it';
is $b.v, 9, 'the write reached the object';
