use Test;

# From CSS::Writer's t/node-doco.t: `lives-ok { $out = EVAL $code }, "name" and
# is $out, ...`. The arguments after a leading block belong to the listop, so the
# loose word-logical ends them; they were parsed as full expressions, which
# swallowed `and is ...` into the second argument and ran `is` before the block.
plan 11;

my @log;
sub f(*@a) { @log.push: @a.elems; True }

@log = ();
f { 1 }, "x" and f 3;
is-deeply @log, [2, 1], '`and` ends the arguments after a block';

@log = ();
f { 1 }, 5 xor f 3;
is-deeply @log, [2, 1], '`xor` ends them too';

@log = ();
f { 1 }, 5, 6 and f 3;
is-deeply @log, [3, 1], 'several arguments, then `and`';

@log = ();
f { 1 }, 5 or f 3;
is-deeply @log, [2], '`or` short-circuits after the call';

@log = ();
f { 1 }, (1 and 2);
is-deeply @log, [2], 'a parenthesised `and` stays inside the argument';

my $out;
lives-ok { $out = 7 }, "assigns" and is $out, 7, 'the block ran before the second call';

for 1..2 -> $i {
    my $q;
    lives-ok { $q = $i * 2 }, "loop $i" and is $q, $i * 2, "loop value $i";
}

# vim: expandtab shiftwidth=4
