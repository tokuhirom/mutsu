# Five fixes that let upstream NativeCall's `is native` path run

With `use NativeCall` switched to the vendored upstream module (measured on the unmerged branch
`exp/11203-nativecall-interception-off`), the first `is native` call failed on a chain of
general interpreter bugs. With all five fixed below, `sub strlen(Str --> size_t) is native {*};
strlen("hello")` returns 5 through upstream's own `trait_mod:<is>`, replacement body and
`nqp::nativecall` (#11203).

- **Role-body lexicals of a role parameterised on a Sub** (#11528). A parameterised role's
  body lexicals are stored under its pun class's storage name. For an argument whose
  spelling is not its identity (a Sub, a Block), that name carries the argument's `.WHICH`.
  Method dispatch on a mixin rebuilt the name without that suffix, so upstream's
  `Native!setup` read its `INIT my Lock $setup-lock` as Nil and silently never built the call.
  Both sides now share `parametric_role_pun_name`.
- **`&f does R`** now rebinds `&f`, as `$x does R` rebinds `$x` (#11459). A later `&f` used to be
  a fresh rebuild of the declared sub, which kept the role markers but lost the role's
  attribute store.
- **A closure made by a role method** reads the role's live attributes even after that method
  returned: `method mk { -> { self!set; $!n } }` on a routine or value the role is mixed into.
  Upstream's replacement body reads `$!arity` and `$!rettype` this way.
- **A private role attribute** (`has str $!name`) is no longer reachable as an accessor on a
  mixin. On a routine with upstream's `Native` role mixed in, `.name` returned the empty
  attribute instead of the routine's name.
- **The routine a `trait_mod:<is>` candidate receives** is now built the way `&name` builds it,
  so it carries the declared return type. `$r.signature.returns` had read `Mu`, so upstream
  marshalled every return as `void`.
- **`nqp::getattr($capture, Capture, '@!list')`** and **`'%!hash'`** answer a Capture's
  positionals and nameds.

Two findings are filed separately: upstream's `check_routine_sanity` still warns about
`--> size_t`, and an unknown method on a Sub answers a composed-method object instead of dying.
