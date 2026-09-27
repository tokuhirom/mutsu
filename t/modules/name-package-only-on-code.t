use Test;

# `.name` and `.package` are `Code` methods (plus the container descriptors'
# `.name`), not something every object answers (#9776). mutsu used to return
# `Nil`, the value itself or the type name.

plan 14;

throws-like { 3.name }, X::Method::NotFound, message => /"type 'Int'"/, 'Int has no .name';
throws-like { "x".name }, X::Method::NotFound, 'Str has no .name';
throws-like { Any.name }, X::Method::NotFound, message => /"type 'Any'"/,
    'a type object has no .name';
throws-like { (1, 2).name }, X::Method::NotFound, 'List has no .name';
throws-like { my $y; $y.package }, X::Method::NotFound, message => /"type 'Any'"/,
    'an undefined scalar has no .package';
throws-like { Int.package }, X::Method::NotFound, 'a type object has no .package';

sub greet() { }
is &greet.name, 'greet', 'a routine still has its name';
is &greet.package.^name, 'GLOBAL', 'a routine still has its package';
is /a/.name, '', 'an anonymous regex is a Code with an empty name';
is Nil.name, Nil, 'Nil swallows .name';
throws-like { Sub.name }, Exception, message => /'type object'/,
    'a Code type object cannot read the name attribute';

# `.^can` agrees with the call: only `Code` offers `name`.
is True.^can('name').elems, 0, 'Bool.^can("name") is empty';
is Int.^can('name').elems, 0, 'Int.^can("name") is empty';
is &greet.^can('name').elems, 1, 'a routine can .name';
