use v6;
use Test;

# When a stubbed (required) role method and its concrete implementation both
# reach a class through a *shared intermediate role* (a composition diamond),
# the concrete satisfies the requirement -- there is no conflict. mutsu wrongly
# reported "X::Role::Composition::Conflict: multiple candidates for required
# method" because the same underlying definition, re-tagged with the
# intermediate role's origin, was counted twice. T-023 (UpRooted).

plan 4;

# The exact diamond shape from UpRooted: a stub in Quoter (via FQN) and a
# concrete in DBIConn both flow through the single intermediate role Selector.
{
    role Quoter { method !qc ( $v, $t ) { !!! } }
    role FQN does Quoter { }
    role DBIConn { method !qc ( $v, $t --> Str ) { "quoted" } }
    role Selector does FQN does DBIConn { }
    class Reader does Selector { method go { self!qc(1, 2) } }
    is Reader.new.go, 'quoted', 'diamond stub+concrete via one intermediate role resolves';
}

# Deeper chain (class does a role that does the Selector).
{
    role Quoter2 { method !qc ( $v, $t ) { !!! } }
    role FQN2 does Quoter2 { }
    role DBIConn2 { method !qc ( $v, $t --> Str ) { "deep" } }
    role Selector2 does FQN2 does DBIConn2 { }
    role Reader2Role does Selector2 { }
    class Reader2 does Reader2Role { method go { self!qc(1, 2) } }
    is Reader2.new.go, 'deep', 'diamond resolves through two intermediate roles';
}

# Class directly composes the stub role and the concrete role (no intermediate).
{
    role Q3 { method !qc ( $v, $t ) { !!! } }
    role C3 { method !qc ( $v, $t ) { "direct" } }
    class R3 does Q3 does C3 { method go { self!qc(1, 2) } }
    is R3.new.go, 'direct', 'direct stub+concrete resolves (unchanged)';
}

# A genuine conflict -- a stub plus TWO different concrete implementations from
# distinct roles -- must STILL be rejected.
{
    my $err;
    {
        EVAL q:to/CODE/;
            role QS { method !qc ( $v, $t ) { !!! } }
            role CA { method !qc ( $v, $t ) { "A" } }
            role CB { method !qc ( $v, $t ) { "B" } }
            class RC does QS does CA does CB { method go { self!qc(1, 2) } }
            RC.new.go;
        CODE
        CATCH { default { $err = .message } }
    }
    ok $err.defined, 'stub + two distinct concretes still conflicts';
}
