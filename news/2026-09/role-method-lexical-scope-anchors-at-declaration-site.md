# Role methods keep their declaration-site lexical scope through composition

A role nested inside a class may refer to a bareword type that is a sibling
of the role in that same enclosing class:

```raku
class Outer {
    class Inner { }
    role UsesInner {
        method make() { Inner.new }
    }
}
class Consumer does Outer::UsesInner { }
say Consumer.new.make.^name;   # Outer::Inner
```

Rakudo anchors a role method's lexical visibility at the role's own
declaration site, so `Inner` keeps resolving to `Outer::Inner` even after
`UsesInner` is composed into `Consumer`, a class wholly unrelated to
`Outer`. mutsu instead resolved the bare name against the *consuming*
class's own scope, which does not know about `Outer::Inner` at all, and
threw `X::Undeclared::Symbols: Undeclared name: Inner used at line 1`.

Bare type-name resolution for a composed method already had one
role-scope-aware probe (`resolve_suppressed_type` in
`src/runtime/runtime_encoding.rs`): it binds `::?ROLE` to the method's
originating role and tries `{role}::{name}` for a type declared *inside*
the role body itself. That covered a `my class` declared inside the role,
but not a sibling type declared in the role's *own enclosing* package.

The fix widens that probe: since a role's fully-qualified name already
encodes its lexical nesting chain via `::` (`Outer::UsesInner`'s enclosing
package is just `Outer`, found by splitting off the last `::` segment), the
probe now also walks up the role's own qualified name, trying each
enclosing package in turn, the same way it already checks the current
package and the method-class stack. No new storage was needed on `RoleDef`
— the qualified name already is the scope chain — and no change was needed
to how role methods are compiled or composed.

A composed method still only inherits the *role's own* lexical scope chain:
a method the consuming class declares itself continues to resolve bare
names against its own scope, unaffected by whatever the composed role could
see.

Found while measuring `Lumberjack::Dispatcher::Syslog` 0.0.6, whose
`Lumberjack::Logger` role refers to the enclosing `Lumberjack::Message`
class after composition into the test class.

Closes [#8565](https://github.com/tokuhirom/mutsu/issues/8565).
