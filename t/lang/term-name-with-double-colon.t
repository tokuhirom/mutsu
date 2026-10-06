use v6;
use Test;

# A categorical name's bracket group is part of the NAME: the `::` in
# `term:<Foo::Bar>` is not a package qualifier (#11822). Every layer that takes
# a name apart at `::` -- symbol classification, the function-key base-name
# scan, the dispatch resolver, the qualified-symbol fallback -- has to agree,
# or one layer qualifies a name another treats as plain, and the call dies with
# "Could not find symbol '&Bar>' in 'GLOBAL::term:<Foo'" from inside a sub.
#
# Every expectation below was verified against Rakudo.

plan 14;

sub term:<Foo::Bar> () { 1 }
sub infix:<a::b>($x, $y) { $x + $y }
sub prefix:<p::q>($x) { $x * 10 }

is Foo::Bar, 1, 'a term whose name contains :: is callable at the top level';

sub in-sub { Foo::Bar }
is in-sub(), 1, '... from inside a sub';

my $closure = { Foo::Bar };
is $closure(), 1, '... from inside a closure';

is (map { Foo::Bar + $_ }, 1..3).join(','), '2,3,4', '... from inside a map block';

class K { method m { Foo::Bar + 1 } }
is K.new.m, 2, '... from inside a method';

is &term:<Foo::Bar>.name, 'term:<Foo::Bar>', 'the routine keeps its whole name';
is &term:<Foo::Bar>(), 1, '... and is callable through its & form';

is (3 a::b 4), 7, 'an infix whose name contains :: works at the top level';
sub in-infix { 3 a::b 4 }
is in-infix(), 7, '... and from inside a sub';

sub in-prefix { p::q 5 }
is in-prefix(), 50, 'a prefix whose name contains :: works from inside a sub';

# A package in front of the categorical is still a qualifier.
module Pkg {
    our sub term:<In::Pkg> () { 9 }
    our sub peek { In::Pkg }
}
is Pkg::peek(), 9, 'a package-scoped term with :: in its name resolves inside its package';
is Pkg::term:<In::Pkg>(), 9, '... and through its package-qualified spelling';

sub term:<A::B> () { 1 }
sub term:<A::C> () { 2 }
sub both { A::B + A::C }
is both(), 3, 'two terms sharing a leading segment stay distinct';

class C { our sub term:<Foo::Baz> () { 5 }; method m { Foo::Baz } }
is C.new.m, 5, 'a class-scoped term with :: in its name resolves in a method';
