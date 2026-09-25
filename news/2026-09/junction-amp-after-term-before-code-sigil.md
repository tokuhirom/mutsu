# `self &callback` is an all-junction, not a `&callback` term

`GLFW`'s `lib/GLFW/Window.rakumod` contains

```raku
STORE => sub ($, &callback) {
    set-window-refresh-callback(self &callback);
});
```

(almost certainly meant as `self, &callback`, but valid Raku: an all-junction of
`self` and `&callback`). mutsu could not compile the module and failed with this
#7988 cluster's generic `Confused. expected statement: ...` message.

The junction parser refuses a spaced `&` followed by a name
(`parse_junction_infix_op_after`), because after a listop name (`f &g`) that is
a `&g` code-variable argument, not an infix. #7954 already lifted the guard for
the glued form (`Int&Str`). But after a *complete term* — `self`, a variable,
a literal, a method call — a term cannot start either, so a spaced `&name` is
the infix too; rakudo agrees (`my $x = 1; ($x &c).WHAT` is `Junction`). The
junction loop now treats such a left operand like the glued case; only a
bareword or call head (a possible listop) keeps the sigil reading, and `self`
counts as a term although the AST spells it as a bareword.

Pinned by `t/types/junction-all-infix-before-code-sigil.t`. GLFW's
`GLFW::Window` now compiles.
