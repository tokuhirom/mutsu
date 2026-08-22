# Assigning to a readonly *variable*/binding throws `X::Assignment::RO` where raku throws `X::AdHoc`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/containers.rakudoc:137`
and `Language/hashmap.rakudoc:504`).

## Minimal repros

```raku
my $x := 42;
$x = 23;
CATCH { default { say .^name, ': ', .Str } };
```
- `raku`: `X::AdHoc: Cannot assign to an immutable value`
- `mutsu`: `X::Assignment::RO: Cannot assign to a readonly variable (x) or a value`

```raku
my %answers = illuminatus => 23, hitchhikers => 42;
for %answers.values -> $v { $v += 10 };
CATCH { default { put .^name, ': ', .Str } };
```
- `raku`: `X::AdHoc: Cannot assign to a readonly variable or a value`
- `mutsu`: `X::Assignment::RO: Cannot assign to a readonly variable (v) or a value`

```raku
# for comparison: a case where raku DOES use X::Assignment::RO
my $a = 5;
sub f($x) { $x = 10 }
f($a);
CATCH { default { say .^name, ": ", .Str } }
```
- `raku`: `X::AdHoc: Cannot assign to a readonly variable or a value` (also `X::AdHoc`!)

```raku
my constant PI = 3.14; PI = 5;
CATCH { default { say .^name, ": ", .Str } }
```
- `raku`: `X::Assignment::RO: Cannot modify an immutable Rat (3.14)` (this one IS `X::Assignment::RO`)

## Root cause hypothesis

Real Raku distinguishes two different failure categories that mutsu currently conflates
into one `X::Assignment::RO` error:

- **Assigning through a readonly *binding*/alias with no writable container of its own**
  (`:=`-bound literal, a non-`is rw` sub parameter, a `for`-loop variable iterating readonly
  hash values) → real Raku throws the generic **`X::AdHoc`** with message "Cannot assign to
  a readonly variable or a value" (or the containers.rakudoc-specific wording "Cannot assign
  to an immutable value").
- **Modifying an actually-immutable *value*** (a `constant`, a literal List/Array element)
  → real Raku throws the specific **`X::Assignment::RO`** with a "Cannot modify an immutable
  `TYPE` (`VALUE`)" message.

mutsu appears to route every "can't assign to this" case through the same
`X::Assignment::RO` handler regardless of which category applies, so a caller doing
`CATCH { when X::AdHoc { ... } }` around, e.g., a `for %h.values -> $v { $v = ... }` typo
would not catch mutsu's exception the way it catches real Raku's.

## Affected files (starting point)

- Wherever assignment to a non-writable lvalue throws `X::Assignment::RO` (grep
  `X::Assignment::RO` in `src/vm/` and `src/runtime/`) — needs to distinguish "no backing
  container at all" (readonly binding/alias/non-rw parameter/loop variable) from "backing
  container refuses writes because the value itself is immutable" (constant, literal
  list/array element), and throw `X::AdHoc` for the former.
- Related, narrower finding already filed: [bind-scalar-literal-var-name-not-int.md](bind-scalar-literal-var-name-not-int.md)
  (the `$b := 1; $b.VAR.^name` should be `Int` gap) — likely the same underlying "bind-to-
  literal is not modeled as containerless" representation gap.
