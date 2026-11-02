# A unit's own `$=pod` and Str-keyed pairs in `hyper`/`race`

Two bugs found with Sway::Config (its `Sway::Config::CLI` unit failed to load):

- A loaded `unit module Outer::Inner` that had already `use`d `Outer` read the
  **parent's** `$=pod` in its mainline: the unit's own document was only
  registered as a compunit lexical after the mainline ran, so the lexical walk
  climbed to the ancestor package first. It is now registered before the
  mainline starts.
- `.hyper.map(*.key)` / `.race.map(...)` handed a Str-keyed `Pair` element to
  the worker as a *named* argument, so `$/.caps.hyper.map(*.value.made)` died
  with "Too few positionals passed". The element is now passed positionally,
  exactly as `map` does.
