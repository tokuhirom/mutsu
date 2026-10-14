# Fixture for t/modules/import-export/exported-nested-module-wrapped-sub.t:
# the FontConfig::Raw layout -- a `unit module` holding an exported nested
# module whose `our sub` stubs a trait replaces (there: `is native`).
unit module ExportedNestedWrap;

multi trait_mod:<is>(Routine $r, :$replaced!) is export {
    $r.wrap(-> |c { "replaced: {c.list.join(',')}" });
}

module Inner is export {
    our sub stubbed(*@a) is replaced {...}
    our sub plain() { 'plain' }
}
