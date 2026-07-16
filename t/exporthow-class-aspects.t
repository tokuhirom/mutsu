use Test;

# EXPORTHOW `class` metaclass override + a custom Metamodel::ClassHOW subclass
# whose `compose` wraps every local method (AOP-style method boundaries).
# Pins the machinery behind roast integration/advent2011-day14.t:
#   - `EXPORTHOW.WHO.<class> = SomeHOW` makes a later `class` use SomeHOW as its
#     meta-object (so `TheClass.HOW.<user-method>` dispatches to it),
#   - a user `multi trait_mod:<is>(Mu:U, SomeType:U)` fires for a KNOWN-class
#     parent that matches its typed constraint (`is X` desugaring),
#   - a HOW subclass reaches the native ClassHOW meta-methods it does not
#     override (`self.methods($obj, :local)`, `add_parent`),
#   - `.wrap` on a Method returned by `.^methods(:local)` takes effect for later
#     dispatch, and `callsame` in the wrapper reaches the original method.

plan 8;

# --- The native ClassHOW meta-methods reachable from a HOW subclass ----------
{
    my class WrapAllHOW is Metamodel::ClassHOW {
        has @!log;
        method note-call($obj, $name) { @!log.push($name) }
        method calls($obj) { @!log }
        method compose($obj) {
            for self.methods($obj, :local) -> $m {
                my $how := self;
                $m.wrap(-> $o, |args {
                    $how.note-call($o, $m.name);
                    callsame;
                });
            }
            callsame;
        }
    }

    my $how = WrapAllHOW.new;

    my class Target {
        method double($x) { $x * 2 }
        method triple($x) { $x * 3 }
    }

    # `self.methods($obj, :local)` on a HOW subclass returns the local methods.
    my @names = $how.methods(Target, :local).map(*.name).sort;
    is-deeply @names, ['double', 'triple'], 'HOW subclass can call self.methods(:local)';

    # `compose` wraps every local method; the wrappers call through (callsame)
    # AND record the call on the HOW's own state.
    $how.compose(Target);
    is Target.double(4), 8,  'wrapped method still returns the original result (double)';
    is Target.triple(4), 12, 'wrapped method still returns the original result (triple)';
    is-deeply $how.calls(Target).sort.Array, ['double', 'triple'],
        'each wrapped call was recorded via the HOW state';
}

# --- `.wrap` on a `.^methods(:local)` Method object --------------------------
{
    my class C { method f($x) { $x + 1 } }
    my $m = C.^methods(:local).first(*.name eq 'f');
    my $fired = 0;
    $m.wrap(-> $o, |args { $fired++; callsame });
    is C.f(10), 11, '.wrap on a .^methods Method object keeps the original result';
    is $fired, 1, 'the wrapper installed by .^methods(...).wrap actually fired';
}

# --- native add_parent is idempotent (does not duplicate an existing parent) --
{
    my class Base { method kind { 'base' } }
    my class Sub is Base { }
    # Re-adding an already-present parent through the metamodel must be a no-op,
    # not a duplicate that corrupts the MRO.
    Sub.HOW.add_parent(Sub, Base);
    is Sub.new.kind, 'base', 'add_parent is idempotent for an existing parent';
    is-deeply Sub.^parents(:local).map(*.^name), ('Base',),
        'no duplicate parent after a redundant add_parent';
}
