use Test;

plan 5;

# `.join` stringifies each element with `.Str`; a nested list's `.Str`
# stringifies ITS elements the same way, so a user `method Str` deep inside
# still runs (Terminal::Print joins a grid of rows of `Cell` objects).
class D { method Str { 'y' } }
my class L { method Str { 'x' } }

is ([D.new],).join, 'y', 'a user Str inside a nested array';
is ([D.new], D.new).join('-'), 'y-y', 'nested and direct instances together';
is [[D.new, D.new], [D.new]].join('|'), 'y y|y', 'rows of instances';
is ([L.new],).join, 'x', 'a lexical class inside a nested array';
is ([1, 2], [3]).join('|'), '1 2|3', 'plain nested lists are unchanged';
