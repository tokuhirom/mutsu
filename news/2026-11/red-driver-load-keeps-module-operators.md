# Modules loaded inside a block keep their operators; CompUnit matcher and resource fixes

Three fixes from Red's migration-bundle and dependency-discovery tests:

- A scope that declared or loaded routines restores the routine registry when
  it exits, including the gate that records which compilation units see a
  user `infix:<op>`. A module first loaded inside that scope (Red loads its
  SQLite driver with `require ::($driver-name)` from `database`) lost the
  operators it had imported for its own code, so the driver's
  `self.type eq "table"` fell back to the core `eq` on every later call and
  `.grep(*.is-table)` compiled to `WHERE 0`. The restore now rolls back only
  the scope's own unit.
- `CompUnit::DependencySpecification.new` normalizes its matchers as rakudo
  does: an undefined matcher is `True`, a version or api matcher becomes a
  `Version`, an auth matcher is kept. Red builds a spec with
  `:api-matcher(Any)` when no `:api<>` is given, which matched nothing.
- `.content` of an installed distribution's resource answers a handle on
  `<prefix>/resources/<id>`, where `install` stores it, instead of
  `<prefix>/<id>`.
