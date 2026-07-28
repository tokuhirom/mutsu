# A positional parameter de-itemizes, and a user method outranks a same-named builtin

Two independent dispatch/binding bugs, both surfaced by the same construct: the
YAMLish battery's block-collection actions. With them fixed, `load-yaml` returns
what raku returns for every block sequence and mapping.

## An `@`-sigiled parameter de-itemizes its argument

An `@` parameter is a *positional binding*: whatever Positional it is handed
becomes the array's **elements**. An itemized one (`$(1,2)` — what a
list-assignment destructure leaves in a scalar) therefore de-itemizes. mutsu did
that for an ordinary `:@a`, but the attributive form wrote the value straight
through to the attribute:

```raku
class S { has @.elems; submethod BUILD(:@!elems) {} }
my ($cls, $elems) = (S, (1, 2));    # $elems is itemized
$cls.new(:$elems).elems             # raku: 2 elements.  mutsu: held the list itself
```

Stored raw, `@!elems` ended up holding the list, so iterating the attribute
yielded that list instead of its elements — `.map(*.concretize(…))` over
YAMLish's `Sequence` called `concretize` on the `Array` rather than on each
`Node`. `bind_param_value` now normalizes an itemized Positional for any
`@`-sigiled parameter name. A plain attribute assignment (no BUILD) still keeps
the item, because that is an assignment and not a binding.

Pin: `t/positional-param-deitemizes.t`.

## A user-declared method outranks a same-named builtin

`call_method_with_values` consults three by-name dispatchers
(`dispatch_method_by_name_1/2/3`) that key on the method *name* alone. They ran
before user-method resolution, so a class that declared `map`, `elems`, `sort`,
… could never be reached through that path:

```raku
grammar G { token TOP { <map> }; token map { … } }
class Actions { method map($/) { … } }
G.parse($text, :actions(Actions))
# X::Cannot::Map: Cannot map a Match to a Actions, it's not callable.
```

A grammar dispatches the action named after each rule, and YAMLish's
block-mapping rule is called `map`, so the action was answered by the collection
builtin — which then rejected the Match as a non-callable block. The by-name
dispatchers are now skipped when the target's class declares a method of that
name, checked for the type object as well as an instance (`:actions(Actions)`
passes the class). Nothing else changes: a class that does not declare the name
still gets the builtin.

Pin: `t/user-method-shadows-builtin-name.t`.

## Result

Together with the two fixes that landed earlier the same day — regex `{ }`
side-effect blocks running inline (#5510) and nested type names being qualified
by their enclosing package (#5511) — YAMLish's block collections now round-trip:

```
- 1\n- 2    =>  [1, 2]
a: 1        =>  {a => 1}
a: 1\nb: 2  =>  {a => 1, b => 2}
- x\n- y    =>  ["x", True]
```

identical to raku on all four. The battery's remaining work is packaging
(vendor into `modules/YAMLish/`, the `batteries.lock` + whitelist gate, the
Batteries page row), not interpreter fixes.
