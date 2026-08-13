# X::Multi::NoMatch candidate-signature and arg-profile formatting diverges from raku in three ways

Found while fixing `todo/tickets/signature-gist-invocant-format.md` (ADR-0019 E9-pre follow-up):
that ticket asked to check whether `X::Multi::NoMatch` shares the `Signature.gist` rendering
helper. It does not — `format_method_candidate_signatures`
(`src/runtime/class.rs:298`) and `format_call_arg_profile`
(`src/runtime/class_dispatch.rs:15`) are independent hand-rolled formatters with their own bugs.

## Divergence

```raku
class WorkingTie {
    multi method has_tie(Int $z) { }
    multi method has_tie(Str $z) { }
}
WorkingTie.new.has_tie([1,2,3]);
```

```
# raku:
Cannot resolve caller has_tie(WorkingTie:D: Array:D); none of these signatures matches:
    (WorkingTie $:: Int $z, *%_)
    (WorkingTie $:: Str $z, *%_)

# mutsu:
Cannot resolve caller has_tie(WorkingTie:D: Array); none of these signatures matches:
    (WorkingTie: Int $z, Any *%_, *%_)
    (WorkingTie: Str $z, Any *%_, *%_)
```

Three independent bugs in the mutsu output:

1. **Invocant format**: `WorkingTie:` instead of `WorkingTie $::` — the same divergence just
   fixed in `Signature.gist` (`src/value/signature.rs`), but `format_method_candidate_signatures`
   builds its own `"({}: ", receiver_class_name)` string rather than going through
   `render_param`/`render_signature`, so the fix did not propagate here.
2. **Duplicate slurpy**: `Any *%_, *%_` — the loop's own `*%_` skip filter
   (`class.rs:326-331`) only matches when `pd.name` is `"%_"`, `"_"`, or empty, but apparently one
   of the two synthesized slurpy params in `def.param_defs` has some other name and slips through
   before the explicit `*%_)` is appended at the end.
3. **Missing `:D`/`:U` on positional arg types**: `Array` instead of `Array:D` in the arg
   profile — `format_call_arg_profile` (`class_dispatch.rs:15`) renders positionals via
   `what_type_name(a)` alone; rakudo always shows the definedness smiley for positional args in
   this message (concrete `:D`, type object `:U`), matching the smiley it already computes for
   the invocant (`smiley` in `methods_signature_errors.rs:175`).

## Fix route

The principled fix is to make `format_method_candidate_signatures` build its strings through
`render_param`/`render_signature` (or an equivalent shared code path) instead of duplicating
invocant/slurpy formatting, which would fix (1) and likely (2) together. (3) is a separate,
smaller fix in `format_call_arg_profile`/`what_type_name` call sites to append the smiley for
positional args the same way `make_multi_no_match_error_detailed` already does for the invocant.

Several `t/` tests grep-match `Cannot resolve caller` loosely (e.g.
`t/proto-dispatch-interpreter-path.t`, `t/exception-messages.t`), but none pin this detailed
candidate-list message's literal text, so a fix should add a dedicated pin.
