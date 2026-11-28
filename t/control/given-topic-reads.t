use Test;

# Reads of `$_` in topicalizer bodies take a shortcut straight to the env
# binding (#18274). It must answer exactly what the full read does.

plan 7;

sub kinds($o) {
    with $o { (.WHAT.^name, $_ ~~ Int, $_ + 0).join('|') } else { 'undef' }
}
is kinds(5), 'Int|True|5', 'with body reads its topic repeatedly';
is kinds(Int), 'undef', 'without branch';

given 'abc' {
    is .chars ~ $_, '3abc', 'given topic read twice';
    given 'z' { is $_, 'z', 'an inner given shadows the topic' }
    is $_, 'abc', 'and the outer topic is back after it';
}

my @r = do for <a b> -> $_ { $_ ~ $_ };
is @r.join(','), 'aa,bb', 'a pointy loop parameter named $_';

sub mod-topic() { my $s = 'q'; given $s { $_ = 'w' }; $s }
is mod-topic(), 'w', 'assigning the topic writes back to its source';
