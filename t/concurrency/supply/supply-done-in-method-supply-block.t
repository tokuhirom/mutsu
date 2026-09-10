use Test;
plan 5;

# `done` inside a `supply { ... }` block desugars to `$emitter.done(); <terminator>`
# (src/parser/primary/ident/supply.rs). The terminator used to be a routine
# `return`, which — when the closure was created inside a *method* — got
# stamped with that method's callable id (via the captured
# `__mutsu_callable_id` env entry) and escaped past the (long-returned) method
# frame as an uncaught `CX::Return`, killing the tap/react with a quit instead
# of a normal `done`. A closure created in a plain sub had no such id, so it
# happened to work — this file pins both shapes.
# See todo/tickets/supply-done-in-method-supply-block-escapes-as-cx-return.md.

class A {
    method pp() { supply { done } }
}

{
    my $saw-value = False;
    react { whenever A.new.pp() -> $x { $saw-value = True } }
    nok $saw-value, 'react whenever on a method-returned supply { done } never fires (empty supply)';
    pass 'react completed without dying on the CX::Return quit';
}

{
    my $quit-reason;
    my $done = False;
    A.new.pp().tap(-> $ { }, done => { $done = True }, quit => -> $r { $quit-reason = $r });
    ok $done, '.tap on a method-returned supply { done } fires the done callback';
    nok $quit-reason.defined, '.tap does not see a CX::Return quit';
}

# emit before done still delivers its values (the desugar's terminator must
# not swallow prior statements' effects).
class B {
    method qq() { supply { emit 1; emit 2; done } }
}
{
    my @got;
    react { whenever B.new.qq() -> $x { @got.push($x) } }
    is @got.join(','), '1,2', 'emit then done in a method supply block delivers all values';
}
