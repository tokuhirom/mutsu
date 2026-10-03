# Test::Describe: six dispatch and binding gaps

An ecosystem roulette draw of Test::Describe (an RSpec-style test DSL) turned
up six independent gaps. With them fixed, `t/01-basic` and `t/03-pars` pass.
`t/04-change` waits on #11196, `.VAR.name` of an aliasing parameter.

- **Applicability probes leaked parameters into the caller.** A multi taken
  as a value from a package stash dispatches through its captured
  candidates. It tested each candidate by binding its parameters into the
  *current* env. A re-exported `multi ok(Mu $cond, $desc = '')` called
  inside `subtest` overwrote the subtest's own `$desc`, so every nested
  subtest lost its name. The probe now runs on a saved env.
- **`for @its -> &it { it |%p }`.** A `&name` loop parameter now shadows a
  same-named routine, here the module's exported `it`. A `&name` bound to an
  object with a `CALL-ME` can also be called by its bare name.
- **Named dynamic parameters.** `:$*x` was bound only by the light call path,
  so a callee's `$*x` lookup never found it. Its `named_names` was also
  `*x` rather than `x`.
- **`sub EXPORT { Mod::EXPORT::ALL:: }`.** An EXPORT hook returning a stash
  now imports its symbols. `Stash.Map` and `Stash.Hash` coerce to the symbol
  table.
- **`gather { $.take-subs }`.** This no longer warns "Useless use of
  $.take-subs in sink context": `$.name` is a method call on `self`.
