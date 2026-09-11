use Test;
use lib 't/lib';

# Two bugs found while making the Test::Async distribution load:
#
# 1. `class C is export` inside a `unit package P;` whose compunit is named
#    something else (`P::Other`) did not export the bare name. mutsu aliased a
#    module's short type names only when the qualified name sat under the
#    COMPUNIT's name, so `UnitPkgExport::Event` (declared in the compunit
#    `UnitPkgExport::Event`... but reached via `P::`, not `P::Other::`) was
#    filtered out and `Event` resolved to a bareword `Str`. A `sub` with the
#    same `is export` in the same compunit always worked.
#
# 2. A role whose method parameters are typed by such an import raised
#    `X::Parameter::InvalidType` ("Invalid typename 'Event:D' in parameter
#    declaration."): the declaration-time pre-pass deferred an unresolvable
#    QUALIFIED name supplied by a body `use`, but not an unqualified one — and
#    an exported type is unqualified by construction.

plan 10;

use UnitPkgExport::Event;

ok Event.DEFINITE.not, 'the exported class name resolves to a type object';
is Event.^name, 'UnitPkgExport::Event', 'and it is the class the module declared';
is Event.new(id => 3).describe, 'event 3', 'the imported class is usable';
is Event::Report.^name, 'UnitPkgExport::Event::Report',
    'a compound name under the exported one still resolves';
ok Event::Report.new(id => 1, message => 'x') ~~ Event, 'and it inherits from it';
is event-tag(), 'UnitPkgExport', 'a sub `is export` from the same compunit still imports';

# A type the module did NOT export stays invisible.
ok ::('NotExportedAtAll') ~~ Failure, 'an undeclared name is still not resolvable';

use UnitPkgExport::Consumer;

class Handler does UnitPkgExport::Consumer { }

is Handler.new.handle(Event.new(id => 7)), 'definite: event 7',
    'a role method typed by the imported class registers and dispatches';
is Handler.new.handle(Event, id => 9), 'type: event 9',
    'the type-object candidate of the same multi dispatches too';

# The pre-pass must still reject a genuine typo when no body `use` can excuse it.
throws-like q:to/CODE/, X::Parameter::InvalidType, 'a bogus role param type is still rejected';
    role R { method m(NoSuchTypeHere:D $x) { $x } }
    class C does R { }
    CODE
