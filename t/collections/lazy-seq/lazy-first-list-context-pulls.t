use Test;

# `first` on a lazy sequence pulls until an element matches, and `list`/`List`
# of a live `gather` keep it pullable: both are pulls of the lazy step
# (ADR-13891), so an expression receiver, a variable receiver and a call
# through the method object all answer alike. Checked against Rakudo.

plan 10;

my @seen;
sub g() { gather { take 1; @seen.push(5); take 5; @seen.push('late'); take 9 } }

is g().first(* > 2), 5, 'first on an expression';
is-deeply @seen, [5], '... pulls no further than the match';

@seen = ();
my $v = g();
is $v.first(* > 2), 5, 'first on a variable';
is-deeply @seen, [5], '... pulls no further than the match';

@seen = ();
my $m = Seq.^find_method('first');
is $m(g(), * > 2), 5, 'first through the method object';
is-deeply @seen, [5], '... pulls no further than the match';

is (1, 2 ... *).first(:k, * > 3), 3, 'first(:k) on an infinite sequence';

my @got;
my $dies = gather { take 1; take 2; die "boom" };
for $dies.list { @got.push($_); last if $_ == 2 }
is-deeply @got, [1, 2], 'list of a gather stays pullable before a later die';

is gather { take 1; take 2 }.List.^name, 'List', 'List of a gather reports List';
is-deeply (gather { take 3; take 4 }).list.List, (3, 4), '... and reifies to its elements';
