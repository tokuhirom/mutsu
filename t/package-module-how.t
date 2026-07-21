use v6;
use Test;

plan 9;

# A bare `package`/`module` reports its own metaclass, not the default ClassHOW.
package P { }
is P.HOW.^name, 'Perl6::Metamodel::PackageHOW', 'package uses PackageHOW';

module M { }
is M.HOW.^name, 'Perl6::Metamodel::ModuleHOW', 'module uses ModuleHOW';

class C { }
is C.HOW.^name, 'Perl6::Metamodel::ClassHOW', 'class still uses ClassHOW';

# .gist of the metaobject (what `say P.HOW` prints).
is P.HOW.gist, 'Perl6::Metamodel::PackageHOW.new', 'PackageHOW gist';
is M.HOW.gist, 'Perl6::Metamodel::ModuleHOW.new', 'ModuleHOW gist';

# A versioned module exposes .^ver / .^auth / .^name.
my module Mv:ver<1.2.3>:auth<me> { }
is Mv.^name, 'Mv',        'module .^name';
is-deeply Mv.^ver, v1.2.3, 'module .^ver';
is Mv.^auth, 'me',         'module .^auth';

# A package has no version metamethods (absent by design).
my package Pv:ver<1.2.3> { }
dies-ok { Pv.^ver }, 'package .^ver is absent';

done-testing;
