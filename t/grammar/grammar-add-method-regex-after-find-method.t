use Test;

# From Grammar::PrettyErrors: `self.^find_method('ws')` before the grammar has a
# `ws`, then `^add_method('ws', &regex)`: the later lookup finds the regex.
plan 4;

grammar G { rule TOP { a b } }
my regex whitespace { <!ww> \s* }
ok G.^find_method('ws').defined || True, 'lookup before (miss or inherited)';
G.^add_method('ws', &whitespace);
G.^compose;
is G.^find_method('ws').^name, 'Regex', 'the added regex is found';
ok G.parse("a b").defined, 'and a rule uses it';

class C { }
C.^find_method('foo');
C.^add_method('foo', my method foo { 42 });
C.^compose;
is C.new.foo, 42, 'a class method added after a miss works too';
