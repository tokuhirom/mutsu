use Test;

plan 2;

class Box {
    has $.value;
}

ok Box.new(value => [1, 2]) eqv Box.new(value => [1, 2]),
    'user-defined instances compare by their attributes';

class Node {
    has Node $.next is rw;
}

my $left = Node.new;
my $right = Node.new;
$left.next = $left;
$right.next = $right;
lives-ok { $left eqv $right },
    'structural instance equality terminates on cycles';
