use Test;

# A List literal holds a not-yet-run `.map`/`.grep` Seq as an unread element,
# as rakudo does: building the List (and sinking it as a statement) runs no
# callback; reading, flattening or iterating the element does. Expected values
# from rakudo (#17749, found on MCP::Server::Tool::Web t/12).

plan 12;

my @log;
sub m($tag) { (1..2).map({ @log.push: "$tag$_"; $_ }) }

@log = ();
m('a'), m('b');
is-deeply @log, [], 'a sunk List of map Seqs runs none of them';

@log = ();
sub g2 { (m('s'), 3); 1 }
g2();
is-deeply @log, [], 'a List statement inside a routine runs nothing';

@log = ();
sub g(--> Nil) { (m('n'), 3) }
g();
is-deeply @log, [], 'a --> Nil routine ending in such a List runs nothing';

@log = ();
my $l = (m('l'), 3);
is-deeply @log, [], 'assigning the List to a scalar runs nothing';
is $l.elems, 2, 'the List has two elements';
is-deeply @log, [], '...and counting them runs nothing';
is-deeply $l[0].List, (1, 2), 'reading the element runs its map';
is-deeply @log, ['l1', 'l2'], '...once';

my @y = ((1..2).map(* * 10), 3);
is-deeply @y.map(*.List).List, ((10, 20), (3,)), 'an array assignment keeps the Seq element whole';
is-deeply (((1..3).grep(* > 1), 9)).flat.List, (2, 3, 9), '.flat reads the grep';

my @seen;
for ((1..2).map(* + 100), 7) -> $e { @seen.push: $e }
is-deeply (@seen.elems, @seen[0].List, @seen[1]), (2, (101, 102), 7),
    'for iterates the List, the Seq is one element';

my ($p, $q) = ((1..2).map(* + 5), 3);
is-deeply ($p.List, $q), ((6, 7), 3), 'a list assignment destructures it';
