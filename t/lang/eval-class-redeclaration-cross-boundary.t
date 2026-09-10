use Test;

# A top-level (package-scope) class already declared outside `EVAL` cannot be
# redeclared by an `EVAL`'d string, even though the `EVAL` is a separate
# compilation unit -- both install into the same GLOBAL package stash.
{
    class CrossEvalFoo { }
    throws-like { EVAL q[class CrossEvalFoo { }] }, X::Redeclaration,
        'EVAL cannot redeclare a class already declared at top level';
}

# A class declared inside a `sub` body still installs into the enclosing
# package's stash once that sub runs (matches real raku: `class Foo {}`
# anywhere in a package is package-scoped, not scoped to the surrounding
# control-flow block), so it too blocks a later `EVAL` redeclaration.
{
    sub cross-eval-declares-local() { class CrossEvalLocal { } }
    cross-eval-declares-local();
    throws-like { EVAL q[class CrossEvalLocal { }] }, X::Redeclaration,
        'EVAL cannot redeclare a class a sub body already registered';
}

# A `my class` (lexical) declaration is scoped to its own lexical block and
# never installs into the package stash, even after that block has exited --
# it must NOT block an unrelated later `EVAL` from declaring a *package*
# class of the same name.
{
    { my class CrossEvalLexicalLeak { } }
    lives-ok { EVAL q[class CrossEvalLexicalLeak { }] },
        'EVAL may declare a package class whose name a now-exited `my class` used';
}

# `my class` INSIDE the `EVAL`'d string is still allowed to shadow an outer
# package class of the same name (pre-existing behavior, re-pinned here
# alongside the new cross-boundary cases).
{
    class CrossEvalShadow { }
    lives-ok { EVAL q[my class CrossEvalShadow { }; 1] },
        'a lexical class inside EVAL may shadow an outer package class';
}

# Two sibling `EVAL`s each declaring the same (non-lexical) class name both
# install into the same shared GLOBAL package, so the second is a genuine
# redeclaration -- verified against real `raku`, which also errors here even
# though each `EVAL` is its own compilation unit.
{
    EVAL q[class CrossEvalSibling { }];
    throws-like { EVAL q[class CrossEvalSibling { }] }, X::Redeclaration,
        'two sibling EVALs declaring the same class name is a redeclaration';
}

# `eval-lives-ok`/`eval-dies-ok` (unlike `throws-like`) run their code
# without inheriting the caller's context (mirroring real raku's own
# Test.rakumod: `eval_exception` calls plain `EVAL($code)`, while
# `throws-like` explicitly passes `context => $caller-context`), so a class
# they declare does NOT conflict with a same-named class the caller already
# declared -- verified against real raku for both helpers.
{
    class CrossEvalLivesOk { }
    eval-lives-ok 'class CrossEvalLivesOk { }',
        'eval-lives-ok may redeclare an outer class name without conflict';
}
{
    class CrossEvalDiesOk { }
    eval-dies-ok 'class CrossEvalDiesOk { }; die "boom"',
        'eval-dies-ok may redeclare an outer class name without conflict';
}

done-testing;
