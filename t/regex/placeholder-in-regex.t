use Test;

# A placeholder interpolated into a regex literal (`/$^a/`) is a parameter of
# the enclosing block, and one inside a regex code block (`/<?{ $^a }>/`) is
# rejected: that block takes no signature (mutsu#10542).

plan 8;

my &f = { so 'abc' ~~ /$^a/ };
is &f.arity, 1, 'interpolated placeholder counts toward the arity';
ok f('b'), 'and binds the argument the regex matches';
nok f('z'), 'a non-matching argument fails the match';

my &g = { ~('abc' ~~ rx/$^x/) };
is g('c'), 'c', 'rx// literal';

my &h = { ~($^s ~~ m:i/$^p/) };
is h('b', 'ABC'), 'B', 'm:i// with a second placeholder, in name order';

my &k = { so 'a$^b' ~~ /'$^b'/ };
is &k.arity, 0, 'a quoted $^b in a regex is literal text';

throws-like 'my &f = { so "abc" ~~ /<?{ $^a }>/ }', X::Placeholder::Block,
    'placeholder inside a regex code assertion';
throws-like 'sub s { "a" ~~ /a { $^q }/ }', X::Placeholder::Block,
    'placeholder inside a regex code block of a routine';
