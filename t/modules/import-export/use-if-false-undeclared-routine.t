use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

plan 9;

# #10331 (ADR-0134 §2.1.6): a `use Foo:if(False)` imports nothing, so the
# names the parse-time scan registered for it do not declare a routine. A call
# to one is rakudo's compile-time "Undeclared routine", raised after the
# BEGIN-time effects (so the condition is known) and before the mainline.

is_run ｢use lib 't/lib'; use if; use UseIfUndeclaredFx:if(False); say "ran"; say fx()｣,
    %(:out(''), :err(/'Undeclared routine:' \s+ 'fx used'/), :status(1)),
    'a routine only a False conditional use exported is undeclared at compile time';

is_run ｢use lib 't/lib'; use if; use UseIfUndeclaredFx:if(False); BEGIN note "begin"; say "ran"; fx 3｣,
    %(:out(''), :err(/^ 'begin' \n .* 'Undeclared routine:' \s+ 'fx used'/), :status(1)),
    'the check runs after the BEGIN-time effects, and catches a paren-less call';

is_run ｢use lib 't/lib'; use if; use UseIfUndeclaredFx:if(True); say fx()｣,
    %(:out("42\n"), :err(''), :status(0)),
    'a True conditional use imports the routine';

is_run ｢use lib 't/lib'; use if; BEGIN my $c = True; use UseIfUndeclaredFx:if($c); say fx()｣,
    %(:out("42\n"), :err(''), :status(0)),
    'a condition a BEGIN set is honoured';

is_run ｢use lib 't/lib'; use if; use UseIfUndeclaredFx:if(False); use UseIfUndeclaredFx:if(True); say fx()｣,
    %(:out("42\n"), :err(''), :status(0)),
    'a later True use of the same module imports the routine';

is_run ｢use lib 't/lib'; use if; use UseIfUndeclaredFx:if(False); sub fx { 7 }; say fx()｣,
    %(:out("7\n"), :err(''), :status(0)),
    'a routine the unit declares itself is not affected';

is_run ｢use lib 't/lib'; use if; use UseIfUndeclaredGx:if(True); say gx()｣,
    %(:out("7\n"), :err(''), :status(0)),
    'a call into a loaded conditional module is fine';

is_run ｢use lib 't/lib'; use UseIfUndeclaredUser; say "ran"; say call-fx()｣,
    %(:out(''), :err(/'Undeclared routine:' \s+ 'fx used'/), :status(1)),
    'a module is checked the same way when it is loaded';

# `use if` itself imports no routine, so it no longer turns the check off.
is_run ｢use if; say "ran"; nosuch()｣,
    %(:out(''), :err(/'Undeclared routine:' \s+ 'nosuch used'/), :status(1)),
    'a unit that uses only import-free pragmas is still checked';
