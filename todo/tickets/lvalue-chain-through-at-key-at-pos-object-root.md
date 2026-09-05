# An lvalue subscript chain rooted at a subscriptable OBJECT is not routed through AT-KEY/AT-POS

When the root of an lvalue subscript chain is an accessor that returns an
**object** implementing `AT-KEY`/`AT-POS` — rather than a plain Array/Hash —
the write does not reach it:

```raku
class Q { has %.d is rw; method AT-KEY($k) is rw { %!d{$k} } }
class U { has Q $.query = Q.new(d => {foo => [1,2]}) }
my $u = U.new;
$u.query<foo>[0] = 99;
say $u.query.d;
# raku:  {foo => [99 2]}
# mutsu: Cannot subscript-assign through %!query: it returned Q, not an Array
#        or Hash container
```

Measured 2026-09-04 against `raku` v2026.06.

## History

This shape has never worked. Before
`news/2026-09/method-rooted-lvalue-subscript-chain-writes-through.md` it was
handled by `__mutsu_index_assign_method_lvalue_nested`, whose `Instance` branch
looked for a `Proxy` element behind `AT-POS`/`AT-KEY` and, finding none, fell
through to a typed-attribute check that then fired on the *outer* object — so
the same code answered a nonsensical `Type check failed for an element of
@query ...; expected Q but got Hash`. The `:=`-bound spelling
(`my $t := $u.query; $t<foo>[0] = 99`) dropped the write silently.

Now that the chain is routed through the variable-rooted walk, the failure is
at least loud and honest: the walk refuses a root that is not a container.

## What a fix needs

The chain walk (`exec_index_assign_expr_nested_op` /
`exec_index_assign_deep_nested_op` in `src/vm/vm_var_assign_index_named.rs`)
descends only into `Array`/`Hash`/`ContainerRef` roots. For an `Instance` root
it would have to call the user's `AT-KEY`/`AT-POS` — in **lvalue** mode, so the
returned element is a container it can store through (an `is rw` method, or a
`Proxy`). That is the same "a method must be able to return an lvalue
container" problem as `todo/deep/native-method-cannot-return-an-lvalue-container.md`,
and it should be designed with it rather than special-cased here.

Do NOT fix this by reintroducing an accessor-keyed slow path: the deleted one
is exactly what dropped the writes this ticket's neighbours were about.

## Re-verified 2026-09-05

Still reproduces on `main` at `e4994a3`, with the same loud error
("Cannot subscript-assign through %!query: it returned Q, not an Array or Hash
container"). The dependency named above has also not moved: the
2026-08-31 deep-triage note on
`todo/deep/native-method-cannot-return-an-lvalue-container.md` says a bounded
design campaign for universal container-reference propagation has to happen
before an implementation slice can be selected, and no such campaign has
started. So this ticket is still blocked on that design, not on effort.
