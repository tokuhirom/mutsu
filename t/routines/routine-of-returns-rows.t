use Test;

# Code.of and Code.returns are rows of the method table (ADR-12523).

plan 10;

sub f(--> Int) { }
my &g = -> $x { };
multi mm(Int $a --> Str) { }
multi mm(Str $a) { }
class C { method m(--> Str) { } }

is &f.of.raku, 'Int', 'declared return type';
is &f.returns.raku, 'Int', 'returns is the same row';
is &g.of.raku, 'Mu', 'a pointy block has no return type';
is { 1 }.of.raku, 'Mu', 'a bare block has no return type';
is C.^lookup('m').of.raku, 'Str', 'a method object';
is &mm.of.raku, 'Mu', 'a multi dispatcher';
is /a/.of.raku, 'Mu', 'a regex is Code and answers Mu';
is /a/.returns.raku, 'Mu', 'Regex.returns';
is &say.of.raku, 'Mu', 'a builtin routine answers Mu';
is &infix:<+>.returns.raku, 'Mu', 'a builtin operator answers Mu';
