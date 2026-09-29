use Test;

# A user `circumfix:<...>` receives its whole semilist as ONE positional
# argument, and a pair inside it is data, never a named argument. Found via
# BSON::Simple's test helper `sub circumfix:<⦃ ⦄>(|c) { Hash::Ordered.new(|c) }`,
# whose `⦃ hello => 'world' ⦄` built an empty hash because the pair went to
# `new` as a named argument.

plan 9;

sub circumfix:<⦃ ⦄>(|c) { c.raku }

is ⦃ hello => 'world' ⦄, '\("hello" => "world")', 'a lone pair is a positional Pair';
is ⦃ :a(1) ⦄, '\("a" => 1)', 'a lone colonpair is a positional Pair';
is ⦃ a => 1, b => 2 ⦄, '\((:a(1), :b(2)))', 'a comma list is one List argument';
is ⦃ 1, 2 ⦄, '\((1, 2))', 'a positional comma list is one List argument';
is ⦃ 1, ⦄, '\((1,))', 'a trailing comma still makes a List';
is ⦃ ⦄, '\(())', 'an empty circumfix passes the empty List';

sub circumfix:<` `>(*@args) { @args.join('-') }
is `1, 2, 3`, '1-2-3', 'a slurpy parameter flattens the one List argument';

sub postcircumfix:<⟪ ⟫>(|c) { c.raku }
is 5⟪ a => 1 ⟫, '\(5, "a" => 1)', 'a postcircumfix pair operand is positional';

sub circumfix:<⟦ ⟧>(|c) { my %h = |c; %h<x> }
is ⟦ x => 42 ⟧, 42, 'the pair operand arrives as data';
