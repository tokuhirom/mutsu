use Test;

plan 5;

# Sibling top-level blocks/subs that reuse the same `&`-sigil lexical name
# corrupted forward-reference resolution: `Compiler::inherit_outer_code_var_names`
# threaded `self.local_map.keys()` (a MONOTONIC map that deliberately keeps a
# name mapped to its slot after the declaring scope closes, so a later
# sibling can reuse the slot) down to a child closure as its set of visible
# `&`-sigil outer code vars. A block reusing an earlier sibling's `&`-name
# wrongly inherited that stale mapping as "already in scope", routing the
# reference through the compiled-outer-var path instead of the dynamic
# name-lookup fallback the first (genuinely forward-referencing) sibling
# itself correctly used -- producing "Unknown function: f" instead of the
# expected value.
#
# Fixed by filtering against `self.local_scopes` (the live scope-frame
# stack, correctly popped when a sibling block/sub closes) instead of the
# monotonic `local_map`.

{
    # The exact repro from the bug ticket: two sibling bare blocks, each
    # declaring `&g`/`&f` under the same names, `&g`'s closure forward-
    # referencing `&f` by bare call.
    {
        my &g = -> { f() };
        my &f = -> { 100 };
        is g(), 100, 'first sibling block: forward call reference resolves';
    }

    {
        my &g = -> { f() };
        my &f = -> { 200 };
        is g(), 200, 'second sibling block reusing the same &g/&f names still resolves';
    }
}

{
    # The sub-declaration variant: two top-level subs, each independently
    # declaring `my &g`/`my &f` under the same names.
    sub outer1 {
        my &g = -> { f() };
        my &f = -> { 100 };
        g();
    }
    sub outer2 {
        my &g = -> { f() };
        my &f = -> { 200 };
        g();
    }
    is outer1(), 100, 'first sibling sub: forward call reference resolves';
    is outer2(), 200, 'second sibling sub reusing the same &g/&f names still resolves';
}

{
    # Three separately-compiled sibling blocks in a row, to pin that the fix
    # isn't a one-shot "only the second block works" accident.
    my @results;
    {
        my &g = -> { f() };
        my &f = -> { 10 };
        @results.push(g());
    }
    {
        my &g = -> { f() };
        my &f = -> { 20 };
        @results.push(g());
    }
    {
        my &g = -> { f() };
        my &f = -> { 30 };
        @results.push(g());
    }
    is-deeply @results, [10, 20, 30], 'three separate sibling blocks in a row each resolve their own &f';
}
