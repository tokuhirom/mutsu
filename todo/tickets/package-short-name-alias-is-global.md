# A `class A::B` binds the bare name `B` globally, not in package `A`

Declaring a class whose name is already qualified installs its short name into
the *global* env, so it resolves from anywhere:

```raku
class Cro::Hdr { }
say Hdr.^name;   # raku: Undeclared name 'Hdr'   mutsu: Cro::Hdr (was), Str (now)
```

In raku the class is installed into package `Cro`, so bare `Hdr` resolves only
from inside that package. In mutsu the binding is a plain env entry, which means
it is visible everywhere *and* it shadows a later same-short-name declaration in
an inner scope:

```raku
class Cro::Hdr {
    my grammar Hdr { token TOP { \w+ } }
    method check(Str $s) { so Hdr.parse($s) }
}
my $s = Supplier.new;
my $b = supply {
    my enum E2 <X Hdr Y>;
    whenever $s.Supply { emit Hdr.key }     # raku: Hdr    mutsu: dies
};
react {
    whenever $b -> $v { say $v; done }
    whenever Promise.in(0.3) { $s.emit(1) }
}
```

This is the live blocker behind `Cro::HTTP::ResponseParser` and
`Cro::HTTP::RequestParser`: both declare `my enum Expecting <StatusLine Header
Body>` inside their `transformer` supply block, and `Cro::HTTP::Header` is a
class whose short name is `Header`. The enum member loses, and the parsers die
with `X::Undeclared::Symbols: Header`, which is why
`t/http-response-parser.rakutest` sits at 129/154 and
`t/http-request-parser.rakutest` at 93/108.

## Where it comes from

`exec_register_class_op` (`src/vm/vm_typedecl_ops.rs`), the `!parent_is_class`
branch:

```rust
if qualified_name.contains("::") && !parent_is_class {
    let short = /* last :: segment */;
    self.env_mut().entry_or_insert_with(short, || Value::package(...));
}
```

## Why the obvious gate does not work

Restricting the alias to declarations whose `current_package` is the declaring
package (so a file-scope `class Cro::Hdr` gets none) breaks `URI`: `class
URI::Path` is declared at file scope in `URI/Path.rakumod`, and `unit class URI`'s
methods legitimately write `my Path $path`. Their `current_package` at method-run
time is not `URI` either (method dispatch only re-points `current_package` when
the owner class has class-scoped subs, package lexicals, or a `::` in its name).

The real fix is to make the alias *package-scoped* — install it in the declaring
parent package's stash / `package_type_aliases` and let
`resolve_type_in_current_package` find it by walking the package chain — and to
make method dispatch anchor `current_package` to the owner class so that walk
starts in the right place. That second half is the same
`current_package`-during-method question several other tickets touch, so it wants
its own design pass.

## Related

`todo/tickets/class-nested-my-class-clobbers-outer-short-name.md` was the
*class-body* half of this problem and is fixed — a type declared in a class body
now has its short-name binding restored when the body ends, and nested
classes/roles/subsets are recorded as class-scoped short names so the class's own
methods still resolve them (`t/class-body-type-scope.t`). This ticket is the
remaining *package* half.
