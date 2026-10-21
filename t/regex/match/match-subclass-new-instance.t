use Test;

plan 8;

class MyMatch is Match { }
my $m = MyMatch.new(:orig('hello'), :from(1), :pos(3));
is $m.from, 1, '.from of a Match subclass built with .new';
is $m.to, 3, '.to';
is $m.Str, 'el', 'the matched substring is orig[from..to]';
is $m.orig, 'hello', '.orig';
is $m.^name, 'MyMatch', 'the class is the subclass';
ok $m.Bool, '.Bool';
is $m.list.elems, 0, 'no positional captures';
ok $m ~~ Match, 'it is a Match';
