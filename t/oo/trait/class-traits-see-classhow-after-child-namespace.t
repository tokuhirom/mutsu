use Test;

# A child module (`Foo::Util`) vivifies the namespace `Foo` as a package. When
# `Foo.rakumod`'s `unit class Foo` is loaded afterwards, its method traits see
# `$*PACKAGE.HOW` as the class's own ClassHOW, not the vivified PackageHOW
# (Method::Also's `is also<Str gist>` then composes its aliases; CSS::Properties).

plan 2;

use lib 't/lib';
use ChildNsProbe::Util :answer;
use ChildNsProbe;

is ChildNsProbe.new.seen.join(','), 'Perl6::Metamodel::ClassHOW',
    'a method trait sees ClassHOW after a child module made the namespace';
is ChildNsProbe.new.write, 'written', 'the class is usable';
