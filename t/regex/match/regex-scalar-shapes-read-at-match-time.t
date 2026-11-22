use Test;

# Every bound `$var` of a pattern is a match-time atom (ADR-12917 direction
# 2, #14510): it is read when the cursor reaches it, whichever entry point
# parses the pattern and whatever the variable's shape. The interpolation
# pre-pass splices only constants (inlined at compile time, as on rakudo).

plan 10;

# A package-qualified scalar a code block assigns.
package P { our $v = "a" }
is ~("xb" ~~ / x { $P::v = "b" } $P::v /), 'xb', 'a package-qualified scalar is read when reached';

# The variable of a `$foo=ATOM` alias, which drops its atom.
my $x = "a";
is ~("xb" ~~ / x { $x = "b" } $x=[ y ] /), 'xb', 'an aliased scalar is read when reached';
is $x, 'b', '... and the block wrote it';

# `Nil` and a type object match nothing.
my $n := Nil;
nok "a" ~~ / a $n /, 'a Nil value matches nothing';
my $u;
nok "a" ~~ / a? $u /, 'a type object matches nothing';
my $s = Str;
nok "a" ~~ / a $s /, 'a typed type object matches nothing';

# A placeholder of the enclosing block.
is { ~("ab" ~~ / a $^p /) }("b"), 'ab', 'a placeholder interpolates';

# `<<` before a variable is the word boundary, not a `<$var>` assertion.
my $w = "A";
is ("A + B" ~~ /<<$w/).Str, 'A', '<<$var matches at a word start';

# A pattern parsed by a non-match entry point reads at match time too.
my $c = "a";
is "xb".comb(/ x { $c = "b" } $c /).join, 'xb', '.comb reads the block\'s write';

# A hash stays rejected at parse time.
my %h;
my $h = %h;
throws-like { "a" ~~ / $h / }, X::Syntax::Reserved, 'a hash value is still reserved';
