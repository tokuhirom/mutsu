# Red's AST identity checks stop paying for the probe chain

Red's `Red::AST.WHICH` (`ValueObjAt.new: "{ self.^name }|{ $.args>>.WHICH.join: "|" }"`)
runs for every hash key and optimizer comparison, and each evaluation recursed
through `Red::Column.gist` (`$!attr.package.HOW.^can("as") ?? $!attr.package.^as !! ...`).
Most of the cost was dispatch overhead, not work. A single `.WHICH` on an `Eq`
node took about 1.42M instructions and now takes far fewer. Red's t/157 went
from 176 s to about 130 s on a 4-core container; raku takes 53 s.

What changed:

- **Hyper `>>.WHICH` / `>>.WHY` on objects.** These are ordinary methods, so a
  class's own `WHICH` now takes the compiled user-method dispatch for every
  element, as `$obj.WHICH` has since #17580. It used to take the general
  `call_method_mut_with_values` path with a temporary env binding. An element
  that arrives in a shared cell (an attribute read `$!l, $!r`, a `.grep` alias)
  takes that dispatch too.
- **A wrap of one method no longer turns off the lanes of every other.** The
  `.^name`, `.HOW`/`.WHAT`, `.^m`, `clone`, Attribute and plain-method lanes
  declined in any program where some method had ever been `.wrap`ped. Red wraps
  `TWEAK` and `BUILD`. They now decline only for a wrap of the method they
  answer (`Registry::method_name_ever_wrapped`).
- **The cached default metaobject is not a custom metaclass.** `dispatch_how`
  caches the default `Perl6::Metamodel::ClassHOW` instance in
  `class_how_values`, so a class whose `.HOW` had been asked once was treated as
  having a user metaclass, and the `.^name` lane refused it.
  `Registry::class_has_custom_how` tells the two apart.
- **A `.^can(NAME)` lane.** On an ordinary class, including a user metaclass
  instance such as Red's `MetamodelX::Red::Model`, it answers from `mop_can`'s
  memo directly.
- **A sigilless relay.** `method as(Mu \type) { self.table: type }` passes a
  caller's read-only sigilless binding. It now binds as a value on the method
  fast path instead of sending the call to the full binder.
- **`.join` over `ObjAt` elements.** Joining plain `ObjAt` / `ValueObjAt`
  elements renders them natively.
- **`//=`, `||=` and `&&=`.** The container test these operators branch on now
  compiles to a `VarIsWritable` opcode. Before, it resolved an internal routine
  through the function-call fallback on every evaluation (Red's
  `method table is rw { $!table //= ... }`).
