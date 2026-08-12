# `given`/`with EXPR -> $v is rw { ... }` on a hash/array element topic does not write back

Found as a side effect while pinning the `%?RESOURCES{key}` element-source-topic
fix (`with-resources-pseudo-var-element.t`) — not otherwise related to that fix.

`t/given-element-topic.t` already covers writeback through the *implicit*
topic (`given %h<a> { $_ = 99 }` / `given %h<a> { .= uc }`) — the element-source
optimization (`TagElementSource`, `container_var_name` in
`src/compiler/helpers_control_flow.rs`) exists specifically to make that kind
of `$_` mutation propagate back to the source element. But an *explicit*
pointy-block parameter marked `is rw` on the same element-source topic does
NOT write back:

```raku
my %h = a => 1, b => 2;
given %h<a> -> $v is rw { $v += 10 }
say %h<a>;   # raku: 11 — mutsu: 1 (unchanged)

my %h2 = a => 1, b => 2;
with %h2<a> -> $v is rw { $v += 10 }
say %h2<a>;  # raku: 11 — mutsu: 1 (unchanged)
```

Both `given` and `with` show the same gap (they share the
`container_var_name`/`TagElementSource` machinery), so the bug is most
likely that the explicit `-> $v is rw` pointy-parameter binding path doesn't
know about (or bypasses) the `TagElementSource`-tagged element-writeback
channel that the implicit-`$_`-mutation path uses — probably in whichever
code binds the pointy parameter to the topic value (`is_copy_topic` /
`pointy_routes_through_given` handling in
`src/parser/stmt/control/with_stmt.rs`, and the corresponding `given`
compilation in `src/compiler/stmt.rs` around the `element_source` block).

Reproduce with `tmp/with-rw-elem-check.raku`, no fixtures/modules needed.

## Next step

Read how `-> $v is rw` binds against a `given`/`with` topic when
`element_source` is `Some(...)` (`src/compiler/stmt.rs` around line 2765) —
compare against how the implicit-`$_`-mutation writeback fires (likely a
different opcode/end-of-block check) and make the explicit rw-param binding
go through the same writeback channel.
