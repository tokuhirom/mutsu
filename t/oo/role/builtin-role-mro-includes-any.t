use v6;
use Test;

plan 5;

class Plain does Positional { }

is Plain.^mro.map(*.^name).join(','), 'Plain,Any,Mu',
    'a composed built-in role is not a class MRO entry';
ok Plain.isa(Any), 'a class composing a built-in role inherits Any';
ok Plain.new ~~ Any, 'an instance composing a built-in role inherits Any';
is Plain.^mro(:roles).map(*.^name).join(','), 'Plain,Positional,Any,Mu',
    'the built-in role is exposed by the :roles MRO';
ok Plain.new ~~ Positional, 'the built-in role remains composed for type checks';
