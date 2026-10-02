use v6;
use lib 't/lib';
use Test;

# From Deps (zef distribution): `use Deps::Item::Store` (a `unit class`) made the
# bare name `Store` resolve to that class in the importing compunit, hiding an
# enum value `Store` imported from another module, so multi dispatch on the enum
# value failed ("Cannot resolve caller lm").
plan 5;

use DepsShort::Dispatcher;

my $d = DepsShort::Dispatcher.new;
is $d.go(DepsShort::LC::{'Sto'}), 'S', 'enum value dispatches in a method multi (Sto)';
is $d.go(DepsShort::LC::{'New'}), 'N', 'enum value dispatches in a method multi (New)';
is $d.lm(DepsShort::LC::New), 'N', 'direct call';

use DepsShort::Item::Sto;
ok DepsShort::Item::Sto.new.v == 1, 'the class is still usable by its full name';
throws-like { EVAL 'Sto.new' }, X::Undeclared::Symbols, 'the short name is not imported into the user';
