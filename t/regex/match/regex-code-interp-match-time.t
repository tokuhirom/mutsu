use Test;

# `$( code )` / `@( code )` contextualizers, and a `"…$x.meth()…"` chain in a
# double-quoted atom that no compiled qq thunk covers, are evaluated when the
# atom is matched, on the running interpreter (#10157) — not by a scratch
# interpreter while the pattern text is built.

plan 15;

my $x = "ab";

# A pattern declaring `:my` gets no qq thunks, so its `"…$x.uc()"` atom
# takes the match-time path.
is ~("yxAB" ~~ / :my $y = 1; y "x$x.uc()" /), 'yxAB', 'method chain in a "..." atom';
is ~("yXABXABz" ~~ / :my $q; y "X$x.uc()"+ z /), 'yXABXABz',
    'a quantifier after the "..." atom binds to the whole literal';

is ~("a2b" ~~ / a $(1+1) b /), 'a2b', '$(...) matches its value literally';
is ~("abab!" ~~ / $("ab")+ '!' /), 'abab!', '$(...) can be quantified';
is ~("AB" ~~ m:i/ $("ab") /), 'AB', '$(...) honors :i';
is ~("a+b" ~~ / a $("+") b /), 'a+b', 'metacharacters in the value are literal';
is ~("x)y" ~~ / x $(")") y /), 'x)y', 'a paren inside a string in the code';
is ~("x(y" ~~ / x @("(", ")") y /), 'x(y', 'a paren inside a string in @(...)';

sub f($s) { $s.flip }
is ~("cba" ~~ / $(f('abc')) /), 'cba', '$(...) calls a sub';

my @w = <foo foobar>;
is ~("foobar" ~~ / @(@w) $ /), 'foobar', '@(...) is an alternation over the elements';
my $re = rx/ \d+ /;
is ~("x12" ~~ / x @($re, 'q') /), 'x12', 'a Regex element of @(...) matches as a regex';

# Probe Q (ADR-0046): `@(...)` ends the declarative LTM prefix.
is ~("StrictX" ~~ / @(<Strict Lax>) 'X' | 'St' /), 'St', '@(...) terminates the LTM prefix';

is ("aaa" ~~ m:g/ $('a') /).elems, 3, '$(...) inside m:g';

# The value is read when the regex is matched, not when it was built.
my $v = 'a';
my $r = rx/ ^ $($v) $ /;
$v = 'b';
ok 'b' ~~ $r, '$(...) reads the variable at match time';
nok 'a' ~~ $r, '...and not its value when the regex was built';
