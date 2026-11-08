# A user role named `Grammar`, and helper tokens in a slang's grammar role

The first two walls EEL (#16888) hit, found by shrinking its `EXPORT` into a probe:

- A role group named `Grammar` (`our role Grammar[Mu $x]`, as `EEL::Slang::Grammar` is under its
  package) broke every later `grammar` declaration: the class's default parent `Grammar` was read as
  a role and the group's stub candidate was picked ("No matching candidate found for the parametric
  role"). The core `Grammar` an `is`-parent names is the class, and `.^parents` keeps listing it.
- A slang's grammar role may define helper tokens (`token eel-word { ... }`) that only its own
  productions call; activation no longer refuses them as unsupported overrides. A new rule nothing
  of the slang's refers to, and a `:sym<...>` production of a category the parser does not hook, are
  still refused.

EEL itself still needs the slang host to run a user `method statementlist` that delegates to the
host parser.
