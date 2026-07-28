# `$obj[i].attr = v` no longer replaces the object with its container

The compiler lowers `$obj[i].attr = v` into two steps: resolve `$obj[i]`,
mutate the element's attribute, then write the element back with
`$obj[i] = element`. That write-back reached the plain Array/Hash
element-assign whenever the object declared no `ASSIGN-POS`/`ASSIGN-KEY`, and
that path **replaces** the variable with a freshly built container. The object
silently became an `Array`:

```raku
class Elem { has $.a is rw; }
class Box does Positional {
    has @!c handles <AT-POS elems>;
    submethod BUILD() { @!c := Array[Elem].new(:shape(2)); @!c[0] = Elem.new; @!c[1] = Elem.new }
    method tag() { "box" }
}
my $b = Box.new;
$b[0].a = 11;
say $b[0].a;    # 11        — the assignment itself always worked
say $b.tag;     # raku: box   mutsu: No such method 'tag' for invocant of type 'Array'
```

The read kept working because it went through the array; only a method call
revealed that the variable's binding had been swapped out.

The element is already mutated in place through the instance, so by the time
the write-back runs it has nothing left to do. It is now dropped for a plain
user object instead of clobbering the variable. Rakudo rejects a *direct*
`$obj[0] = v` on such an object with "Cannot modify an immutable ..."; this
path cannot distinguish the synthesized write-back from a real assignment, so
it stays silent rather than throwing on the former.

Deciding what counts as "a plain user object" asks the class registry
(`user_declared_classes` plus an MRO check for container subclasses) rather
than enumerating built-in container names. A first attempt did enumerate them
and missed that `"hi".encode` is a `utf8` — a name no container list mentions —
which let an immutable `$blob[0] = 200` silently succeed instead of throwing.
The registry question has no such blind spot.

Pin: `t/object-subscript-attr-assign-keeps-var.t`.

With this and the parameterised-role pun composition landed just before it,
[ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)
P1's stated acceptance is met: `NativeHelpers::CStruct`'s `LinearArray` matches
raku's output exactly — it allocates, computes its element stride from the
role-body `my int $sol = nativesizeof(T)`, indexes, nativecasts, assigns
element fields and disposes.
