unit module ExportedGrammarBase;

# Fixture for t/modules/import-export/unit-module-imported-parent.t.
grammar BaseG is export {
    token TOP  { <word> }
    token word { \w+ }
}

class BaseC is export {
    method hi { 'hi' }
}
