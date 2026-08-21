use v6;
use Test;

# `is TraitName<a b>` is sugar for `is TraitName(<a b>)`: a bareword
# word-list literal immediately following a variable trait name (no
# intervening whitespace) is folded into the trait's argument expression,
# exactly like the parenthesized form. Multi-word `<a b>` evaluates to a
# List; single-word `<a>` evaluates to a plain Str (matching how `<...>`
# behaves everywhere else in the language).

plan 6;

my @log;

multi sub trait_mod:<is>(Variable:D \v, :@restricted!) {
    @log.push(@restricted.raku);
}
multi sub trait_mod:<is>(Variable:D \v, :$tag!) {
    @log.push($tag.raku);
}
multi sub trait_mod:<is>(Variable:D \v, :$plain!) {
    @log.push("plain:{$plain.raku}");
}

# Multi-word angle-bracket sugar: `is restricted<a b>` == `is restricted(<a b>)`.
my %h is restricted<a b> = a => 1, b => 2;
is @log.pop, '("a", "b")', 'multi-word `is Trait<a b>` passes a List argument';
is-deeply %h, { a => 1, b => 2 }, 'the variable itself is still declared correctly';

# Single-word angle-bracket sugar: `is tag<a>` == `is tag(<a>)`, a bare Str.
my $x is tag<a> = 5;
is @log.pop, '"a"', 'single-word `is Trait<a>` passes a Str argument';
is $x, 5, 'the variable itself is still declared correctly';

# Regression: a bareword trait with no argument at all must still work.
my $y is plain = 7;
is @log.pop, 'plain:Bool::True', 'bareword `is Trait` (no argument) still dispatches with no arg';
is $y, 7, 'the variable itself is still declared correctly';

done-testing;
