use v6.e.PREVIEW;
use Test;

# A `Format` is a `Str` subclass that is also a `Callable` (ADR-11276 §9.153):
# its own methods are rows, and every method it inherits from `Str` and `Cool`
# answers on its string.

plan 20;

my $f = Format.new("%d-%s");

# Its own rows.
is $f.raku, 'Format.new("\%d-\%s")', 'Format.raku';
is $f.arity, 2, 'Format.arity';
is $f.count, 2, 'Format.count';
is Format.new('%d-%x').directives, <d x>, 'Format.directives';
is $f(1, "a"), '1-a', 'Format.CALL-ME';
is $f.Callable.arity, 2, 'Format.Callable';
ok $f.AST ~~ RakuAST::Node, 'Format.AST';

# What it inherits from `Str` and `Cool`.
ok $f ~~ Str, 'a Format is a Str';
is $f.chars, 5, 'Str.chars';
is $f.uc, '%D-%S', 'Cool.uc';
is $f ~ '!', '%d-%s!', 'concatenation reads the string';
ok $f eq '%d-%s', 'eq reads the string';
is $f.gist, '%d-%s', 'Str.gist';
is $f.Str.WHAT, Format, 'Str.Str is the receiver itself';

# `Formatter`.
is Formatter.new('%d')(5), '5', 'Formatter.new';
is Formatter.CODE('%x')(255), 'ff', 'Formatter.CODE';
ok Formatter.AST('%d') ~~ RakuAST::Node, 'Formatter.AST';

# A user class that shadows a built-in name is an ordinary class.
{
    my class Instant { }
    is Instant.^mro.map(*.^name).join(','), 'Instant,Any,Mu',
        'a declared class shadowing a built-in inherits from Any';
    my class Format { has $.format }
    is Format.new(:format<%d>).raku, 'Format.new(format => "\%d")',
        'a user Format renders as a user class';
    nok Format.new(:format<%d>) ~~ Str, 'a user Format is not a Str';
}
