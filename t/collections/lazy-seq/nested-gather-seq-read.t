use Test;

plan 10;

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

# `.raku` of a collection holding an unpulled gather renders its elements
# (#13026: a plain gather is a Seq, so no `LazyList` stopgap is involved).
# Expected values are the `raku` output.
my @a = (1, gather { take 2; take 3 });
is @a.raku, '[1, (2, 3).Seq]', 'an Array element';
is (1, gather { take 2 }).raku, '(1, (2,).Seq)', 'a List element';
my %h = a => gather { take 5 };
is %h.raku, '{:a($((5,).Seq))}', 'a Hash value';
is [gather { take 1 }, 2].raku, '[(1,).Seq, 2]', 'an Array literal element';
is (1 => gather { take 9 }).raku, '1 => (9,).Seq', 'a Pair value';
