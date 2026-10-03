use Test;

plan 5;

# A `gather` Seq that sits inside a list (a variable, a hyper result) is
# pulled when the list is read, like any other Seq element.
class Node { has $.v; method all { gather { take $!v; take "$!v!" } } }
my @nodes = Node.new(v => 1), Node.new(v => 2);

is-deeply @nodes>>.all.flat.List, (1, '1!', 2, '2!'), 'hyper results flatten';
is (@nodes>>.all).gist, '((1 1!) (2 2!))', 'hyper results render as Seqs';
my $s = gather { take 5 };
is ($s, 1).gist, '((5) 1)', 'a gather in a variable renders inside a list';
is "{($s, 1)}", '5 1', 'and stringifies inside a list';
is (gather { take 5 }, 1).gist, '((5) 1)', 'a literal gather element';
