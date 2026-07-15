use v6;
use Test;

plan 8;

sub foo (Int $i, @stuff, $blah = 5) { ... }   #OK not used

# Literal defaults render in .signature.raku (advent2009-day20)
is &foo.signature.raku, ':(Int $i, @stuff, $blah = 5)',
    'signature.raku renders literal default';

# @-sigil parameter type is Positional
is &foo.signature.params[1].type.raku, 'Positional', 'untyped @param .type is Positional';
is &foo.signature.params[2].type.raku, 'Any', 'untyped $param .type is Any';

sub typed (Int @x, Str %h, &cb) { ... }   #OK not used
is &typed.signature.params[0].type.^name, 'Positional[Int]', 'Int @x type is Positional[Int]';
is &typed.signature.params[1].type.^name, 'Associative[Str]', 'Str %h type is Associative[Str]';
is &typed.signature.params[2].type.raku, 'Callable', 'untyped &param .type is Callable';
is &typed.signature.raku, ':(Int @x, Str %h, &cb)',
    'sigil-implied types are not spelled out in signature.raku';

# A map callback that is a stub must not run until the Seq is forced
lives-ok { my $s = map -> $x, $y { ... }, 1..6; }, 'stub map callback stays unevaluated';
