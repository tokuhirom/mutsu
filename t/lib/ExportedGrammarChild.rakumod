unit module ExportedGrammarChild;

# Fixture for t/modules/import-export/unit-module-imported-parent.t: inside a `unit module`,
# a bare parent names the type this module imported, not `ExportedGrammarChild::*`.
use ExportedGrammarBase;

grammar ChildG is BaseG is export {
    token word { \d+ }
}

class ChildC is BaseC is export { }
