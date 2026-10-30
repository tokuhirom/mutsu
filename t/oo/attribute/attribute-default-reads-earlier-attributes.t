use Test;

# An attribute default sees the attributes initialized before it (`$!x`),
# the values given to `.new`, and leaves the enclosing scope's same-named
# variables untouched afterwards.
plan 9;

class Chain {
    has $.a = 1;
    has $.b = $!a + 1;
    has $.c = $!b * 2;
    has @.d = $!a, $!b, $!c;
    has $.e = $!c + 100;
}

my $o = Chain.new;
is $o.a, 1, 'first default';
is $o.b, 2, 'second default reads $!a';
is $o.c, 4, 'third default reads $!b';
is-deeply $o.d, [1, 2, 4], 'an array default reads three earlier attributes';
is $o.e, 104, 'a default reads the one before it';

my $given = Chain.new(:a(10));
is-deeply ($given.a, $given.b, $given.c, $given.e), (10, 11, 22, 122),
    'a value passed to .new feeds the later defaults';

# Another class with a differently shaped attribute set, constructed in between.
class Other { has $.x = 5; has $.y = $!x * $!x; }
is Other.new.y, 25, 'a second class resolves its own attributes';
is Chain.new(:a(3)).c, 8, 'and the first class still does';

my $b = 'outer';
class Shadow { has $.b = 'attr'; has $.copy = $!b; }
is Shadow.new.copy ~ $b, 'attrouter', 'a lexical of the same bare name is not clobbered';
