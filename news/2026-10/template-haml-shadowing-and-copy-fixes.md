# Template::HAML: five scoping, container and regex fixes

Working Template::HAML's suite (#10638) turned up five general interpreter bugs,
each now fixed with a regression test:

- **A user class named like a 6.e builtin owns its methods.** `class Formatter`
  (or `class Format`) in user code had `Formatter.new` answered by the builtin
  sprintf-format compiler, so `.new` returned a Sub and a following method call
  returned a `<composed-method:...>` Sub. The builtin `Formatter`/`Format`
  entry points now stand down when a class of that name is declared.
- **A typed parameter binds an itemized Bool.** A Bool stored into a Hash
  element is itemized, and the light call path's type check looked at the item
  rather than its contents: `sub f(Bool $b --> Str)` called with `%h<k>` died
  with "expected Bool but got Bool". Both fast type checks now see through the
  item.
- **`@!a = @e` copies.** Assigning to an array or hash attribute shared the
  source's container, so `@!a.push` also grew `@e` (and two attributes assigned
  from the same list were one container). The attribute store now detaches a
  shared container, as `my @a = @e` does; `:=` still aliases.
- **`&name` names the module's import.** Inside a module's routine,
  `&name` / `&name(...)` read a same-named `my &name` of a *caller* in another
  compunit, so `method tab-up(|c) { &tab-up(|c) }` (whose module imports
  `sub tab-up`) called through EVAL'd template code's
  `my &tab-up = -> |c { $obj.tab-up(|c) }` recursed until the stack ran out.
  A caller binding from another compunit that the routine did not capture now
  yields to the routine's import (or the registered routine).
- **A quoted `<` in a regex no longer hides the next variable.** The scalar
  interpolation pass skipped `'<'` as the start of an angle construct, so
  `/ '<' $tag /` swallowed `$tag` and never matched.
