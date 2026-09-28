use Test;

plan 7;

my regex prefixed(Regex $prefix) { $prefix \d+ }
throws-like { 'dec789' ~~ / <prefixed: 'dec'> / }, X::TypeCheck::Binding::Parameter,
    message => /"expected Regex but got Str"/,
    'a named regex propagates a parameter type error';
ok 'dec789' ~~ / <prefixed: /d<alpha>+/ > /,
    'a Regex argument still binds and matches';

grammar Typed {
    token TOP { <word: 'dec'> }
    token word(Regex $pattern) { $pattern }
}
throws-like { Typed.parse('dec') }, X::TypeCheck::Binding::Parameter,
    message => /"expected Regex but got Str"/,
    'a grammar token propagates a parameter type error';

my &lexical = token (Regex $pattern) { $pattern };
throws-like { 'a' ~~ / <&lexical: 'a'> / }, X::TypeCheck::Binding::Parameter,
    message => /"expected Regex but got Str"/,
    'an anonymous token value propagates a parameter type error';
ok 'a' ~~ / <&lexical: /a/> /,
    'an anonymous token accepts a Regex argument after a failed call';

grammar Multi {
    multi token word(Regex $pattern) { $pattern }
    multi token word(Str $pattern) { $pattern }
    token TOP { <word: 'a'> }
}
ok Multi.parse('a'), 'a compatible multi candidate suppresses another candidate type error';
nok Multi.parse('b'), 'a compatible candidate may fail to match without raising another candidate type error';
