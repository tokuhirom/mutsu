# Python stdlib vs. Raku/mutsu gap analysis

Added `docs/batteries/python-stdlib-comparison.md`, a module-by-module comparison
of Python 3.13's standard library against Raku's core language, Rakudo/mutsu's
by-convention modules (`Test`, `NativeCall`), and mutsu's vendored batteries.
Every module in Python's library reference is classified as core language,
core/bundled, an existing mutsu battery, an unbundled Raku ecosystem module, or a
genuine gap.

The point is to turn "what's missing" from a vague impression into a concrete,
rankable list. A closing summary section names the highest-value gaps worth a
future battery survey — CSV, XML parsing, compression/archiving, a logging
framework, UUID generation, and INI/TOML config parsing — ordered by how often
the underlying need shows up in ordinary scripts, alongside the already-tracked
web-framework slot. `PLAN.md` §1 B1 and `BATTERIES.md` both link to it as the
concrete input for "finalize the bundle list."
