# Manual `Metamodel::ClassHOW` construction: `.HOW.set_why` after `.HOW.compose` fails as "immutable"

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Metamodel/Documenting.rakudoc:30`).

## Repro

```raku
BEGIN {
    our Mu constant Documented = Metamodel::ClassHOW.new_type: :name<Documented>;
    Documented.HOW.compose: Documented;
    Documented.HOW.set_why: do {
        my Pod::Block::Declarator:D $pod .= new;
        $pod._add_leading:  "Documented is an example class for Metamodel::Documenting's documentation.";
        $pod._add_trailing: "Take a look at my WHY!";
        $pod
    };
}

say Documented.HOW.WHY;
```

- raku:
  ```
  Documented is an example class for Metamodel::Documenting's documentation.
  Take a look at my WHY!
  ```
- mutsu:
  ```
  Runtime error: An exception occurred while evaluating a CHECK
  Exception details:
    Cannot modify an immutable 'Documented' type object
  ```

## Root cause hypothesis

This is manual, low-level MOP construction: `Metamodel::ClassHOW.new_type` builds a raw type
object, `.HOW.compose` finalizes/locks it in, and `.HOW.set_why` attaches documentation
metadata to the **HOW (metaclass)** object, not to the type object's own attribute storage —
`set_why` is expected to remain callable after `compose` because it mutates the meta-level
`HOW`, which is a separate, still-mutable object from the composed type. mutsu's `compose`
implementation (or its generic "can I write to this?" immutability check) appears to treat
*any* subsequent `.HOW.*` mutator call on a composed type as forbidden, rather than
distinguishing "mutating the type object's own instance data" (correctly blocked post-compose)
from "mutating metadata held by the HOW object" (should remain allowed for things like
`set_why`).

Note: this is a distinct bug from the more common `.WHY` stringification gap already tracked in
`todo/tickets/pod-why-declarator-object-not-stringified.md` (that ticket covers `Pod::Block::
Declarator.Str`/`.gist` for the normal `#|`/`#=` declarator-comment path; this one is about the
manual `Metamodel::ClassHOW` construction path rejecting `set_why` outright before
stringification is even reached).

## Affected files (starting point)

- Wherever `.HOW.compose`/type-object immutability is enforced (`runtime/class.rs` or the MOP
  implementation) — find the "is this composed/immutable" check that `set_why` (and possibly
  other post-compose `HOW` mutators) trips over, and confirm whether raku's actual semantics
  intentionally exempt metadata-only mutators like `set_why` from the post-compose lock.
