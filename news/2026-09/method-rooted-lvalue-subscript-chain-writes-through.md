# An lvalue subscript chain rooted at a method call writes through

`$o.a[0]<x> = 5` silently lost the write. So did every other method-rooted
lvalue subscript chain that had to autovivify — at any depth >= 2, in both the
`[...]` and the `<...>` spelling, for an array attribute and a hash attribute
alike:

```raku
class A { has @.a }
my $o = A.new;
$o.a[0]<x>    = 5;   # raku [{x => 5}]           mutsu []
$o.a[0][1]    = 5;   # raku [[(Any) 5]]          mutsu []
$o.a[0]<x><y> = 5;   # raku [{x => {y => 5}}]    mutsu []
class H { has %.h }
H.new.h<a><b> = 5;   # raku {a => {b => 5}}      mutsu {}
```

Exit 0, nothing reported. It only survived when the element already existed.

## What was wrong

The compiler rewrote the shape into the `__mutsu_index_assign_method_lvalue_nested`
runtime builtin, which resolved the accessor and then rebuilt both containers
**copy-on-write** before writing the outer one back by variable name. A level
that did not exist yet was created inside that rebuild and never installed back
into the attribute, so it evaporated.

That copy-on-write model is also simply out of date. The accessor already hands
back the attribute's *shared* container — which is why `$o.a.push(1)` reaches
the attribute — and the ordinary **variable**-rooted chain walk autovivifies
into a shared container in place:

```raku
my $t := $o.a;
$t[0]<x> = 5;    say $o.a;   # [{x => 5}] -- correct, all along
```

So the defect was routing, not a missing walker.

## The fix

`compile_expr_index_assign` now evaluates the accessor **once** into a compiler
temp and rebuilds the chain against that temp, so the whole shape goes through
the same opcodes a `my @a` chain does. It is the same temp trick
`compile_expr_method_on_nested_index` already used for `$obj.attr<a><b>.push`.
Only depth >= 2 routes this way; a single-level `$o.a[0] = v` keeps the
accessor write-back path it needs.

Two things had to move for that routing to be sound:

- **The typed-container check.** The builtin was the only place that rejected
  `class A { has Int @.a }; $o.a[0]<x> = 5`, and the variable-rooted path did
  not perform it at all. It now reads the constraint off the container's own
  `value_type` (`news/2026-09/typed-array-nested-autoviv-type-check.md`,
  extended here to hash roots), so it fires for any root — including a compiler
  temp, which has no declaration to consult. The message now matches rakudo's:
  `Type check failed for an element of @!a[0]; expected Int but got Hash`.
- **A loud failure when the accessor does not return a container.** The temp's
  name carries the accessor spelling and a recognisable prefix, so the chain
  walk can tell it apart from a user variable. If such a root is not an
  Array/Hash the write is refused with
  `Cannot subscript-assign through @!a: it returned Any, not an Array or Hash
  container` instead of being dropped. Ordinary variable-rooted chains are
  untouched.

`__mutsu_index_assign_method_lvalue_nested` and its copy-on-write helper are
deleted — about 190 lines of `runtime/methods.rs`-era slow path.

## Measured

A 15-shape table was run against raku v2026.06 before and after. Nine shapes
went from wrong to exactly right; two typed shapes went from a mutsu-specific
message to rakudo's wording; four were already correct and stayed so. The one
remaining divergence is an accessor that returns a **subscriptable object**
rather than a container (`$u.query<foo>[0] = 99` with a user `AT-KEY`), which
was broken in both spellings before this change too — it is filed separately as
`todo/tickets/lvalue-chain-through-at-key-at-pos-object-root.md`.

Pinned in `t/method-rooted-lvalue-subscript-chain.t` (14 tests), passing
verbatim under both `mutsu` and `raku`. Gates: `make test` and a full local
`make roast`.
