use Test;

# `list $s` on a single Seq is rakudo's no-op: it returns the very same Seq
# without touching its iterator. mutsu used to reify the argument first (the
# ADR-0058 deferred-map/grep guard on native-function arguments), and `reify`
# is the one thing that marks a Seq body `retained` — which exempts it from
# every later CONSUMING touch. So one `list $s` permanently disarmed the Seq's
# single-use gate (roast S03-operators/context-forcers.t, "list listop doesn't
# cache"). Every expectation below is measured against raku.

plan 8;

sub consumed(&blk) {
    my $died = False;
    try { blk(); CATCH { default { $died = $_ ~~ X::Seq::Consumed } } }
    $died;
}

# The no-op itself.
{
    my \s = (list 1..5).grep(* > 0);
    ok (list s) =:= s, 'list on a Seq returns the same object';
    isa-ok (list s), Seq, 'list on a Seq is still a Seq';
}

# `list` alone never consumes: two of them in a row are fine.
ok !consumed({ my \s = (list 1..5).grep(* > 0); my $a = list s; my $b = list s; }),
   'list listop twice does not consume a grep Seq';
ok !consumed({ my \s = (1, 2, 4 ... 16);        my $a = list s; my $b = list s; }),
   'list listop twice does not consume a sequence-operator Seq';

# ... and it does not disarm the gate for a later consuming touch.
ok consumed({ my \s = (list 1..5).grep(* > 0); sink s>>.abs; sink s>>.abs; }),
   'hypering a grep Seq twice throws X::Seq::Consumed';
ok consumed({ my \s = (list 1..5).grep(* > 0); my $a = list s; sink s>>.abs; sink s>>.abs; }),
   'an intervening list listop does not disarm that';
ok consumed({ my \s = (list 1..5).grep(* > 0); sink (list s)>>.abs; sink (list s)>>.abs; }),
   'hypering through the list listop twice still throws';
ok consumed({ my \s = (1, 2, 4 ... 16); sink (list s)>>.abs, (list s)>>.abs; }),
   'the same holds for a sequence-operator Seq';
