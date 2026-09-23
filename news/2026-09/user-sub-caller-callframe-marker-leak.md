# A user sub named `caller` or `callframe` no longer receives the internal marker

`sub caller($x) { "c$x" }; say caller(1)` used to die with `Unexpected named
argument '__callframe_line' passed`. The parser attaches an internal
`__callframe_line => N` named pair to every literal `caller(...)`/
`callframe(...)` call — it cannot know at parse time whether the name will
resolve to the builtin or to a user-declared sub of the same name — and the
marker reached a shadowing user sub's signature as a genuine, unexpected
named argument.

The general-binder path already filters an analogous internal marker
(`__mutsu_test_callsite_line`, attached to every test-assertion call) via
`is_internal_named_arg` before binding; `__callframe_line` and
`__callframe_blocks` now go through the same filter. The builtins
(`builtin_caller`/`builtin_callframe`) read these markers straight off the
raw argument list in their own native dispatch path, which never reaches
the general binder, so the real (unshadowed) builtins are unaffected.

See [issue #9093](https://github.com/tokuhirom/mutsu/issues/9093).
