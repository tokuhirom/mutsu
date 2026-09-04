# A typed array now refuses an autovivified nested intermediate

Descending a second subscript through an element that does not exist
autovivifies an `Array`/`Hash` *into* that element, and a typed container has to
refuse it:

```
$ raku -e 'my Int @a; @a[0][1] = 5'
Type check failed for an element of @a[0]; expected Int but got Array ([])
```

mutsu accepted it silently and left `[[(Int) 5]]`. The hash-rooted twin already
threw (`my Int %h; %h<a><b> = 5`); only the array root was missing the check.

## The check is read off the container, not off the declaration

The existing hash check is keyed on the *variable*: it tests
`var_name.starts_with('%')` and looks the constraint up with
`var_type_constraint(&var_name)`. That cannot work for a root that has no
declaration to consult — a `:=`-bound alias, or the shared array an attribute
accessor hands back.

The new array check reads `ArrayData::value_type` off the container the store is
about to autovivify into, so it is root-agnostic:

```raku
class A { has Int @.a }
my $o = A.new;
my $t := $o.a;
$t[0]<x> = 5;     # now: Type check failed for an element of $t[0]; expected Int
```

It returns early — before any allocation — when the root is not a typed array,
when the element already holds a container (nothing is autovivified, the
existing one is descended into), or when the constraint accepts the
intermediate. `my @a`, `my Any @a` and `my Array @a` are all unaffected.

## Why this slice exists

It is the prerequisite for closing
`todo/tickets/method-rooted-subscript-chain-autoviv-is-dropped.md` (Tier S:
`$o.a[0]<x> = 5` silently loses the write). The fix there is to route a
method-call-rooted chain through the *variable*-rooted chain walk, which already
autovivifies into the accessor's shared container correctly. That routing was
blocked because the accessor-keyed slow path
(`__mutsu_index_assign_method_lvalue_nested`) was the only thing performing the
typed-container check; moving the check onto the container removes the blocker,
and fixes the `my Int @a` case on its own.

Pinned in `t/typed-array-nested-autoviv-type-check.t` (10 tests), which passes
verbatim under both `mutsu` and `raku`. Validated with `make test` and a full
local `make roast`, since the change makes mutsu stricter.
