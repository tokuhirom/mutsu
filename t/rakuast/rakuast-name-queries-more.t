use Test;

# `RakuAST::Name`'s `simple-identifier`, `base-name`, `without-colonpairs` and
# `without-first-part`, measured on rakudo 2026.09 (#14916).

plan 12;

use experimental :rakuast;

my $one = RakuAST::Name.from-identifier("foo");
my $multi = RakuAST::Name.from-identifier-parts("Foo", "Bar", "baz");
my $global = RakuAST::Name.from-identifier-parts("GLOBAL", "Foo");

is $one.simple-identifier, "foo", 'simple-identifier of a one-part name';
is $multi.simple-identifier, 0, 'simple-identifier of a multi-part name is 0';
is $one.base-name.canonicalize, "foo", 'base-name';
is $multi.base-name.canonicalize, "Foo::Bar::baz", 'base-name of a multi-part name';
is $multi.base-name.^name, 'RakuAST::Name', 'base-name is a Name';
isnt $multi.base-name.WHICH, $multi.WHICH, 'base-name is a new node';
is $one.without-colonpairs.canonicalize, "foo", 'without-colonpairs';
is $multi.without-first-part.canonicalize, "Bar::baz", 'without-first-part';
is $global.without-first-part.canonicalize, "Foo", 'without-first-part of a GLOBAL name';
is $one.without-first-part.parts.elems, 0, 'a one-part name loses its only part';
is $one.without-first-part.canonicalize, "", 'the empty name spells as ""';
is $one.without-first-part.is-empty, 1, 'the empty name is empty (an Int, as in rakudo)';
