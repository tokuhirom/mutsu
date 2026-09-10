# Assigning to a junction or slice subscript THROUGH a nested container target
# (`%h<x>{any('p','q')} = v`, `%h<x>{@k} = ...`) must autothread per key.
# Previously the nested / generic index-assign ops stringified the multi-key
# subscript into a single garbage entry, so a later read of any real key
# returned Any.
#
# Scope: the OUTERMOST subscript being a junction/slice (the common case),
# across the named-nested, generic, and deep-nested assign ops. A junction in a
# NON-outermost position (`%h<a>{any()}<z> = v`) is a rarer, semantically
# subtler case still left unfixed.
use Test;

plan 14;

# --- named 2-level nested target (IndexAssignExprNested) ---
{
    my %d;
    %d<x> = {};
    %d<x>{any('p', 'q')} = 99;
    is %d<x><p>, 99, 'nested hash: junction assign reaches first key';
    is %d<x><q>, 99, 'nested hash: junction assign reaches second key';
}

{
    # autovivified intermediate
    my %e;
    %e<x>{any('p', 'q')} = 7;
    is %e<x><p>, 7, 'autoviv nested hash: junction assign first key';
    is %e<x><q>, 7, 'autoviv nested hash: junction assign second key';
}

{
    # slice subscript through a nested hash
    my %g;
    %g<x> = {};
    my @k = <p q>;
    %g<x>{@k} = 5, 6;
    is %g<x><p>, 5, 'nested hash: slice assign element 0';
    is %g<x><q>, 6, 'nested hash: slice assign element 1';
}

# --- fully-computed outer target (IndexAssignGeneric) ---
{
    my %store;
    %store<x> = {};
    sub gs() { %store }
    gs()<x>{any('p', 'q')} = 88;
    is %store<x><p>, 88, 'computed target: junction assign first key';
    is %store<x><q>, 88, 'computed target: junction assign second key';
}

{
    my %s2;
    %s2<x> = {};
    sub gs2() { %s2 }
    my @k = <p q>;
    gs2()<x>{@k} = 3, 4;
    is %s2<x><p>, 3, 'computed target: slice assign element 0';
    is %s2<x><q>, 4, 'computed target: slice assign element 1';
}

# --- 3+ level deep nested target (IndexAssignDeepNested) ---
{
    my %h;
    %h<a><b> = {};
    %h<a><b>{any('p', 'q')} = 5;
    is %h<a><b><p>, 5, 'deep nested: junction assign first key';
    is %h<a><b><q>, 5, 'deep nested: junction assign second key';
}

{
    my %g;
    %g<a><b> = {};
    my @k = <p q>;
    %g<a><b>{@k} = 7, 8;
    is %g<a><b><p>, 7, 'deep nested: slice assign element 0';
    is %g<a><b><q>, 8, 'deep nested: slice assign element 1';
}
