# Loop labels are ordinary identifiers again, hyphens and lowercase included

`raku-doc/doc/Type/Label.rakudoc` uses `MY-LABEL:` in its own canonical example:

```raku
MY-LABEL:
for 1..10 {
    next MY-LABEL if $_ < 5;
    print "$_ ";
}
```

rakudo prints `5 6 7 8 9 10 `; mutsu printed nothing.

## Root cause

`is_loop_label_name` (`src/parser/helpers.rs`) accepted only `[A-Z_][A-Z0-9_]*`,
so `MY-LABEL` was not a label. `next_stmt`/`last_stmt`/`redo_stmt` then fell
through to the unlabelled form, leaving the label token — *and its `if`/`unless`
statement modifier* — to be parsed as a separate following statement. So
`next MY-LABEL if COND;` compiled to `next; MY-LABEL if COND;`: an
**unconditional** `next` that fired on every iteration. `last MY-LABEL if COND`
exited on the first iteration, and `redo MY-LABEL unless COND` hung forever.

Label *declarations* never had the restriction — `labeled_loop_stmt` uses the
ordinary `ident()` parser — so only the reference site was broken.

The uppercase rule existed to disambiguate, not because labels are restricted:
rakudo accepts `my-label:` and `Outer:` just as happily. But a shape test can
never get this right, because a loop label is syntactically indistinguishable
from any other bareword.

## Fix

Loop labels are now **registered** at their declaration site. `labeled_loop_stmt`
calls `register_loop_label` once it knows a loop/block keyword follows (so the
`trim: "x"` colon-listop form does not register anything), and
`is_loop_label_name` consults `is_declared_loop_label` first. A label always
appears textually before the body that references it, so the declaration is
already in scope by the time the reference is parsed; registration goes in the
current scope and lookup searches outwards. The three reference sites now gate on
`is_raku_identifier_start` instead of "uppercase or underscore".

The old all-caps shape test is kept as a fallback for labels the registry never
saw (across an `EVAL`/module boundary, or on a form `labeled_loop_stmt` does not
register). It is only consulted for names that are *not* registered, so it can
add label recognition but never take it away.

Hyphenated, lowercase, mixed-case and nested labels now all behave as in rakudo,
and a bare `next if COND` is still unlabelled. Pinned by
`t/custom-operator-and-term-parsing.t` section 5.
