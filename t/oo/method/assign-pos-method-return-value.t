use Test;

# The value of `$obj[$i] = $v` / `$obj{$k} = $v` on a class with its own
# ASSIGN-POS / ASSIGN-KEY is what that method returns. From
# Functional::LinkedList's `my ($f2, $value2) = $f1[0] = 1`.

plan 4;

class P { method ASSIGN-POS($i, $v) { ("node", $v) } }
my $p = P.new;
my ($node, $value) = $p[0] = 9;
is $node, 'node', 'first element of the returned list';
is $value, 9, 'second element';

class H { method ASSIGN-KEY($k, $v) { "$k=$v" } }
my $h = H.new;
is ($h<x> = 3), 'x=3', 'ASSIGN-KEY result';

my @a;
is (@a[0] = 5), 5, 'a plain Array element assignment still yields the value';
