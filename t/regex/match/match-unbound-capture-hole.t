use Test;

# An unbound interior positional capture (an unmatched `(a)?` before a later
# capture) is a hole in rakudo's capture list: the subscript reads answer
# `Nil`, `:exists` is False, and iterating the list yields `Mu` (#10690).
# `Match.keys/.values/.pairs/.kv/.chunks` are Seqs.

plan 23;

my $n = "1b" ~~ / (a)? (\d) /;

# Subscript reads: Nil.
is $n[0].raku, 'Nil', '$m[0] of an unbound slot is Nil';
is $0.raku, 'Nil', '$0 of an unbound slot is Nil';
is $n.list[0].raku, 'Nil', '.list[0] of an unbound slot is Nil';
nok $n.list[0]:exists, '.list[0]:exists is False for the hole';
ok $n.list[1]:exists, '.list[1]:exists is True for a bound slot';
is $n.elems, 2, '.elems counts the hole';
is $n.list.elems, 2, '.list.elems counts the hole';

# Iteration: Mu.
is $n.list.raku, '(Mu, Match.new(:orig("1b"), :from(0), :pos(1)))',
    '.list.raku shows the hole as Mu';
is $n.List.raku, '(Mu, Match.new(:orig("1b"), :from(0), :pos(1)))',
    '.List.raku shows the hole as Mu';
is $n.list.gist, '((Mu) ｢1｣)', '.list.gist shows the hole as (Mu)';
is $n.raku, 'Match.new(:orig("1b"), :from(0), :pos(1), :list((Mu, Match.new(:orig("1b"), :from(0), :pos(1)))))',
    'Match.raku shows the hole as Mu';
my @a = $n.list;
is @a.raku, '[Mu, Match.new(:orig("1b"), :from(0), :pos(1))]',
    'list assignment copies the hole as Mu';
is $n.list.map(*.defined).List, (False, True), '.list.map sees an undefined element';
is $n.values.raku, '(Mu, Match.new(:orig("1b"), :from(0), :pos(1))).Seq',
    '.values yields Mu for the hole';
is $n.pairs.raku, '(0 => Mu, 1 => Match.new(:orig("1b"), :from(0), :pos(1))).Seq',
    '.pairs yields 0 => Mu for the hole';
is $n.kv.raku, '(0, Mu, 1, Match.new(:orig("1b"), :from(0), :pos(1))).Seq',
    '.kv yields Mu for the hole';
is $n.keys.raku, '(0, 1).Seq', '.keys includes the hole index';

# Seq-returning views.
isa-ok $n.pairs, Seq, 'Match.pairs is a Seq';
isa-ok $n.chunks, Seq, 'Match.chunks is a Seq';
isa-ok $n.values, Seq, 'Match.values is a Seq';
isa-ok $n.kv, Seq, 'Match.kv is a Seq';
is $n.chunks.raku, '(1 => Match.new(:orig("1b"), :from(0), :pos(1)),).Seq',
    '.chunks skips the hole';
is $n.gist, "｢1｣\n 1 => ｢1｣", '.gist skips the hole';
