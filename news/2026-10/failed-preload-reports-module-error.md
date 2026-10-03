# A `use` in a package block reports the module's own error when its body dies

`our module M { use Dep; }` with a `Dep` whose body died partway reported
`Redeclaration of routine 'f'` for a sub in `Dep` that is fine. It now
reports the error `Dep`'s body actually raised, as rakudo does (#11351).

A `use` nested in a block is loaded twice. A BEGIN-time `PreloadModule` at
the head of the unit loads it first and keeps quiet about failures, because
it runs before code the load may depend on. The in-place `use` then loads it
again. When the preload's module body dies, it leaves behind what it had
already registered, and the reload trips over those leftovers. The
preload's error is now kept (unless the module was simply not found, which
leaves nothing behind). If the in-place `use` fails too, it reports the
preload's error, the one raised against a clean registry.
