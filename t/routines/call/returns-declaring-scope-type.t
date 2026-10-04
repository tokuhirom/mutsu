use lib 't/lib';
use Test;
use ReturnsProbe;

# A routine's return type is the type its declaring scope names, whoever asks:
# a lexical `my class`, or a `constant` alias of one, still answers when
# `.returns` / `.signature.returns` is read from another module.

plan 3;

my class Lex { method hi { 'lex' } }
my constant Alias = Lex;

sub direct(--> Lex) { Lex }
sub aliased(--> Alias) { Lex }

is returns-probe(&direct), 'lex,lex', 'a lexical class return type';
is returns-probe(&aliased), 'lex,lex', 'a constant alias of it';
ok &aliased.returns === Lex, 'the alias is the aliased type object';
