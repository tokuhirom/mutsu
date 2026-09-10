use Test;
plan 9;

# ADR-0021 P3 (I2): data-minted pairs default positional. A number of
# consumers read a Pair-shaped value from data (not a call boundary) --
# e.g. a variable, a list literal, or a bracketed role argument -- and must
# accept the positional flavour identically to the (now rarer) named one.

# QuantHash .pairs feeding a typed positional parameter (the ADR's original
# motivating bug: Cro::HTTP::Client misbinding headers built this way).
my $bag = bag <a b b c c c>;
my @seen;
sub show(Pair $p) { @seen.push("{$p.key}={$p.value}") }
$bag.pairs.map(&show);
is @seen.sort.join(','), 'a=1,b=2,c=3', '$bag.pairs.map(&show) binds each Pair positionally';

# IO::Path file-test smartmatch with a bare colonpair (not a call argument).
{
    my $f = $*TMPDIR.add("adr0021-pair-consumer-filetest");
    spurt $f, "x";
    ok $f.IO ~~ :e, 'IO::Path ~~ :e (bare colonpair) still matches';
    unlink $f;
}

# `<key>:exists` on a Pair held in a variable.
my $p = a => 5;
ok $p<a>:exists, 'variable-held Pair<key>:exists is True for its own key';
nok $p<b>:exists, 'variable-held Pair<key>:exists is False for another key';

# Hyper op on an expression-level Pair (not a call argument).
is (a => 1) »+» 1, (a => 2), 'hyperop broadcasts into a positional-flavour Pair leaf';

# DateTime.later with a list of adverb-shaped Pairs (not call arguments).
{
    my $now = DateTime.now;
    is-deeply $now.later((:2hours, :30minutes)), $now.later(:2hours).later(:30minutes),
        'DateTime.later(list-of-pairs) matches chained named-arg calls';
}

# Role bracket-argument colonpair (re-parsed standalone, not compiled in
# argument position) still binds a role's named type parameter.
{
    role R[:$a = 1, :$b = $a * 2] {
        method foo { "$a-$b" }
    }
    role S does R[:a(5)] { };
    is S.new.foo, '5-10', 'role bracket colonpair argument binds the named role parameter';
}

# Grammar `.parse(..., :args(...))` with an Array of adverb-shaped Pairs.
{
    grammar H { rule r(:$arg) { { $arg == 42 } } }
    ok H.parse('', :rule<r>, :args(:arg(42),)), ':args(Array-of-pairs) binds the rule\'s named param';
}

# `constant %M` from a single Pair, and subscript-assignment immutability.
{
    my constant %M = (a => 1);
    is %M.WHAT.gist, '(Pair)', 'constant % from a single Pair stays a Pair (not coerced to Map)';
}
