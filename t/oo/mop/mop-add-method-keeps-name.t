use Test;

# A method added with `.^add_method(name, $code)` is installed under `name`
# but keeps its own name: `.^find_method(name).name` and `.^methods` answer
# the code's name, an anonymous one the empty string (#18225).

plan 8;

my $D := Metamodel::ClassHOW.new_type(:name<D>);
$D.^add_method('m', my method dm { 'D.m' });
$D.^compose;
is $D.^find_method('m').name, 'dm', 'a named method keeps its name';
is $D.^methods.map(*.name).join(','), 'dm', '.^methods lists it under that name';
is $D.new.m, 'D.m', 'it is called under the name it was added as';

class E { }
E.^add_method('x', method { 'anon' });
is E.^find_method('x').name, '', 'an anonymous method has the empty name';
E.^add_method('w', sub named-s ($s) { 'sub' });
is E.^find_method('w').name, 'named-s', 'a named sub keeps its name';
is E.new.w, 'sub', 'and is called as the method';
E.^add_method('same', my method same { 'same' });
is E.^find_method('same').name, 'same', 'a method added under its own name';
ok E.^method_table<x>:exists, 'the method table keys the added name';
