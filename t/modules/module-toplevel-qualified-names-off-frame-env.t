use lib 't/lib';
use MONKEY-SEE-NO-EVAL;
use Test;

# The package-qualified names a module declares at its top level -- its
# classes, subsets and enum values (`Pkg::Class`, `Pkg::E::K`, `Pkg::K`) --
# are package symbols, while a private `E::K` belongs to the module's scope.
# ADR-0084 group 2 (#7817) keeps them out of that frame's env; every
# way of reaching them must still see them, from the importer, from the
# module's own code, from a nested routine, from another thread, and through
# the package stash.

use ToplevelQualifiedNames;
use ToplevelQualifiedExport;

plan 19;

is ToplevelQualifiedNames::Color::Green.Str, 'Green', 'Pkg::E::K from the importer';
is ToplevelQualifiedNames::Red.Str, 'Red', 'Pkg::K from the importer';
is ToplevelQualifiedNames::direct(), 'Red,Green,Blue,Red,Dark',
    "the module's own sub reads every spelling";
my $t = ToplevelQualifiedNames::Thing;
is $t.short-enum.Str, 'Green', 'E::K from a method of a module class';
is $t.long-enum.Str, 'Blue', 'Pkg::E::K from a method of a module class';
is $t.pkg-enum.Str, 'Red', 'Pkg::K from a method of a module class';
ok $t.small, 'a module subset by its qualified name';
is (Light, Shade::Dark).map(*.Str).join(','), 'Light,Dark',
    'an exported enum keeps its imported spellings';

is ::('ToplevelQualifiedNames::Color::Blue').Str, 'Blue', 'indirect lookup of Pkg::E::K';
is ::('ToplevelQualifiedNames::Green').Str, 'Green', 'indirect lookup of Pkg::K';
is ToplevelQualifiedNames::<Red>.Str, 'Red', 'Pkg::<K> stash lookup';
is ToplevelQualifiedNames::Color::<Blue>.Str, 'Blue', 'E::<K> stash lookup';
ok ToplevelQualifiedNames::.keys.grep('Green'), 'the package stash lists an enum value';
is EVAL('ToplevelQualifiedNames::Color::Red').Str, 'Red', 'reachable from EVAL';

sub nested() { ToplevelQualifiedNames::Color::Blue }
is nested().Str, 'Blue', 'reachable from a routine the importer declares';
is (start { ToplevelQualifiedNames::Color::Red }).result.Str, 'Red',
    'reachable from another thread';
ok ToplevelQualifiedNames::Thing.new ~~ ToplevelQualifiedNames::Thing,
    'a module class by its qualified name';

# An exported class nested in another class under a qualified name of its own
# is imported by the type object the module throws, not by a fresh package.
is TQE::Diag.^name, 'ToplevelQualifiedExport::TQE::Diag',
    'an exported nested qualified class imports its real type object';
throws-like { ToplevelQualifiedExport.go }, TQE::Diag,
    'and an exception the module throws matches it';
