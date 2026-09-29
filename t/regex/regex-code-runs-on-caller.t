use Test;

# Code the regex engine runs while matching — a `<{ … }>` interpolation, a
# `** { … }` quantifier, a subrule argument expression — runs on the caller's
# interpreter (#10151), so its effects on routine state (a `state` variable)
# are the program's own, not a throwaway copy's.

plan 9;

sub counter { state $n = 0; ++$n }

ok "abc" ~~ / a <{ counter(); 'b' }> c /, '<{ … }> interpolation matches';
is counter(), 2, 'a state variable bumped inside <{ … }> keeps its value';

ok "aaa" ~~ / a ** { counter(); 3 } /, '** { … } quantifier matches';
is counter(), 4, 'a state variable bumped inside ** { … } keeps its value';

my @seen;
sub width { @seen.push('arg'); counter(); 2 }
grammar G {
    token TOP  { <item(width())> }
    token item($n) { \d ** {$n} }
}
ok G.parse("12"), 'parameterized token with a computed argument matches';
ok @seen.elems >= 1, 'the argument expression writes the caller-visible array';
ok counter() > 5, 'a state variable bumped by a subrule argument keeps its value';

# Binding a subrule's parameter must not leave the caller's same-named
# variable marked readonly.
my $value = 'a';
grammar GNamed {
    token TOP { <word(:$value)> }
    token word(:$value) { $value }
}
ok GNamed.parse('a'), 'a named subrule argument binds';
lives-ok { $value = 'b' }, "the caller's same-named variable stays assignable";
