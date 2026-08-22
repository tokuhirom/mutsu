# `::(EXPR)` indirect type constraint in a parameter fails to parse; silently drops the method inside a role body

Found during the XML battery survey (`docs/batteries/xml.md`) while investigating the
`XML` module's (`raku-community-modules/XML`) `t/emitter.rakutest` and `t/make.rakutest`
failures: `No such method 'reparent' for invocant of type 'XML::Element'`.

## Root cause

`XML::Node` (`lib/XML/Node.rakumod` in the `XML` dist) is a role with:

```raku
role XML::Node {
    method reparent(::(q<XML::Element>) $parent) {
        self.remove if $.parent.defined;
        $.parent = $parent;
        self
    }
    ...
}
```

`::(q<XML::Element>)` is Raku's **indirect/dynamic type-name lookup** syntax — resolve a
type at compile time from a string expression, used here (instead of a plain
`XML::Element $parent`) presumably to sidestep a forward-reference / circular-dependency
ordering issue between `XML::Node` and `XML::Element`.

On mutsu this syntax is not accepted as a parameter type constraint:

```raku
class Foo {
    method bar(::(q<Foo>) $x) { say "bar called with $x" }
}
Foo.new.bar(Foo.new);
```

- `raku`: prints `bar called with Foo<...>`.
- `mutsu`: **hard parse failure for the whole file**:
  ```
  ===SORRY!=== Error while compiling ...
  Confused. expected statement: expected expected statement: expected expression
  statement or expression after infix operator or '.' or digits or generic radix
  literal or ...
  ```

But when the same construct appears **inside a `role` body** instead of a top-level
`class`, mutsu does not hard-fail the file — it silently drops just that one method
declaration and continues compiling/running the rest of the file:

```raku
role Bar {
    method reparent(::(q<Foo>) $parent) {
        say "reparent called";
    }
}
class Foo does Bar {}
Foo.new.reparent(Foo.new);
```

- `raku`: prints `reparent called`.
- `mutsu`: `No such method 'reparent' for invocant of type 'Foo'` — i.e. `Foo` composed
  `Bar` successfully, and every *other* method in `Bar` presumably still works, but
  `reparent` itself is simply absent from the MRO. This is the exact symptom `XML`'s own
  test suite hits (`XML::Element does XML::Node`, and `XML::Node::reparent` vanishes).

This is really two related findings about the same underlying parser gap:

1. `::(EXPR)` is not supported as a parameter type constraint at all (should resolve the
   type at compile/run time from the string, same as it already presumably works as a
   *value*-position type-object reference — `$.parent ~~ ::(q<XML::Element>)` inside the
   same file's `previousSibling`/`nextSibling`/`remove` methods does work, since those
   test files get as far as failing on `reparent` specifically, not aborting entirely).
2. The failure mode is **inconsistent by container**: a hard `===SORRY!===` compile abort
   at top level (class), but a swallowed/dropped declaration inside a role. The dropped
   case is more dangerous — it produces a working-looking program that silently omits
   functionality instead of failing loudly, which is a worse failure mode than the parse
   error itself.

## Minimal repros

Top-level (hard parse failure, whole file dies):

```raku
class Foo {
    method bar(::(q<Foo>) $x) {
        say "bar called with $x";
    }
}
Foo.new.bar(Foo.new);
```

Inside a role (silent method drop, file keeps running):

```raku
role Bar {
    method reparent(::(q<Foo>) $parent) {
        say "reparent called";
    }
}
class Foo does Bar {}
Foo.new.reparent(Foo.new);
```

## Why this matters beyond XML

This is a general parser gap (indirect type-name resolution, `Language/typesystem.rakudoc`
covers `::(...)`), not XML-specific — any module using `::(q<TypeName>)` to sidestep a
circular/forward type reference in a method signature will hit this. It blocks 2 of the
`XML` dist's 16 test files outright (`t/emitter.rakutest`, `t/make.rakutest` — both need
`XML::Element.append`, which calls `.reparent`), on top of the separate, more severe
grammar blocker (`todo/tickets/grammar-token-param-dynvar-not-visible-in-subrule.md`)
that blocks the other 14. See `docs/batteries/xml.md` for the full survey.

## Affected files (starting point, not exhaustive)

- `src/parser/` — parameter-type-declaration grammar needs to accept `::(EXPR)` the same
  way it presumably already accepts `::(EXPR)` in expression/value position.
- Whatever swallows the parse error inside a role body instead of propagating it should
  also be tightened regardless of the `::(EXPR)` fix — a role that fails to parse one of
  its methods should not silently compose successfully with that method missing.

Not root-caused further within this survey's time budget.
