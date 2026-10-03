use Test;

# An attribute with a `where` constraint that is left unset holds its undefined
# type object, and Rakudo never runs the predicate against it, so the object
# constructs. A value actually given is still checked.

plan 6;

class T { has $.depends is rw where Positional|Associative }
lives-ok { T.new }, 'an untyped where-attribute left unset';
nok T.new.depends.defined, 'it holds an undefined value';
lives-ok { T.new(depends => ['foo']) }, 'a matching value is accepted';
dies-ok { T.new(depends => 42) }, 'a non-matching value is rejected';

class V { has Int $.y where * > 5 }
lives-ok { V.new }, 'a typed where-attribute left unset';
dies-ok { V.new(y => 3) }, 'a typed non-matching value is rejected';
