use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

plan 6;

# #10977: an error the BEGIN prologue (ADR-0134) raises is a compile-time
# failure in rakudo. The unit never finished compiling, so its END phasers,
# which are queued before the mainline starts, must not run.

is_run ｢BEGIN die "x"; END note "e"｣,
    %(:out(''), :err({ !.lines.grep('e') && .contains('BEGIN') }), :status(1)),
    'a dying BEGIN skips the END phasers';

is_run ｢END note "e"; BEGIN die "x"｣,
    %(:out(''), :err({ !.lines.grep('e') && .contains('BEGIN') }), :status(1)),
    'an END declared before the dying BEGIN is skipped too';

is_run ｢use if; use Foo:if(Any); END note "e"｣,
    %(:out(''), :err(/^ 'Did not provide compile-time-value'/), :status(1)),
    'an undefined `use :if` condition skips the END phasers';

is_run ｢use lib 't/lib'; use if; use UseIfUndeclaredFx:if(False); END note "e"; fx()｣,
    %(:out(''), :err(/^ '===SORRY!===' .* 'Undeclared routine:'/), :status(1)),
    'the undeclared-routine guard after the prologue skips the END phasers';

is_run ｢BEGIN { 1 }; END note "e"; die "y"｣,
    %(:out(''), :err({ .lines.grep('e') && .lines.grep('y') }), :status(1)),
    'a run-time error after the prologue still runs the END phasers';

is_run ｢BEGIN { 1 }; END note "e"; say "ran"｣,
    %(:out("ran\n"), :err("e\n"), :status(0)),
    'a unit with a prologue runs its END phasers on a normal exit';
