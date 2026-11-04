use Test;

# Found via Mathematica::Serializer (Qa-like package names): a package whose
# first segment is a quote-language name plus a fused adverb letter -- `Qa::B`,
# `Qw::B`, `qa::B`, `Qs::B` -- was read as the quote construct (`Q` + adverb `a`)
# with `:` as its delimiter, so the statement died "Confused. Two terms in a
# row". `::` is a package separator and is never a quote delimiter.

plan 10;

class Qa::B::Foo { method hi { 'qa' } }
class qa::B { method hi { 'qa-lower' } }
class Qs::B { method hi { 'qs' } }
class Qf::B { method hi { 'qf' } }

is Qa::B::Foo.^name, 'Qa::B::Foo', 'Qa::B::Foo is a package-qualified name';
is Qa::B::Foo.hi, 'qa', 'and can be called';
is qa::B.^name, 'qa::B', 'qa::B (lower-case quote name)';
is Qs::B.new.hi, 'qs', 'Qs::B';
is Qf::B.hi, 'qf', 'Qf::B';

my constant C = Qa::B::Foo;
is C.^name, 'Qa::B::Foo', 'usable as the value of a constant';

# The real quote constructs keep working.
is-deeply Qw<a b>, ('a', 'b'), 'Qw<a b> is still a word quote';
is-deeply q:w<a b>, ('a', 'b'), 'q:w<a b> is still a word quote';
is-deeply Qww<a b>, ('a', 'b'), 'Qww<a b> is still a word quote with a fused adverb';
is-deeply qw<x y>, ('x', 'y'), 'qw<x y> is still a word quote';
