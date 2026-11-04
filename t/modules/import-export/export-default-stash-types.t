use Test;
use lib 't/lib';
use ExportDefaultStashTypes;

# A type declared directly in a module's own `my package EXPORT::DEFAULT`
# block is part of the default export list, like an `our sub` there: the
# importer sees it under its short name (#16332, Net::IP::Parse).
plan 9;

is StashPlain.new.a, 1, 'a class declared in the stash is imported';
is StashPlain.new.plain, 'plain', '... and its methods work';
is StashMarked.new.b, 6, 'an `is export` class in the stash is imported too';
is StashPlain.^name, 'EXPORT::DEFAULT::StashPlain', 'the type keeps its stash-qualified name';

class Composed does StashRole { }
is Composed.new.r, 'role', 'a role declared in the stash is imported';

is green, 'green', 'enum values of a stash enum are imported';
is StashColor::red, 'red', '... and the enum type name resolves';

my StashPos $p = 5;
is $p, 5, 'a stash subset is imported as a type';
is stash-helper(), 42, 'a stash `our sub` is still imported';
