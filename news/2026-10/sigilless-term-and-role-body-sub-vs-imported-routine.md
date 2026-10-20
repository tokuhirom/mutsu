# Three fixes for a term, an imported routine and a role body that share a name (CSS::Properties)

`CSS::Units` has `role CSS::Units[\dimension, \units] { method dimension{dimension} }` next to an
exported `sub dimension`, and `CSS::Properties::Calculator` imports it (`:&dimension`):

- A bare name at statement start is the sigilless term when one is in scope; the imported-routine
  statement parser (`known_call_stmt`) no longer takes it for a call (`sub g(\dimension) { dimension }`).
- A role parameter bound to a type object (`Lengths`) is a term too: the bareword resolution no longer
  treats the package bound under the name as "the type this routine shares its spelling with" and
  calls the routine instead.
- A role body's `sub ... is export` re-running (every specialization / `Role.method` call) is the same
  declaration, not an `X::Redeclaration` of the alias the first run exported into the importer
  (`Could not instantiate role 'CSS::Units' because it died with X::Redeclaration`; the same signature
  fails Timezones::ZoneInfo, Tinky::JSON and four others in the ledger).
