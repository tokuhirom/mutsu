unit module GrammarSiblingCore;

# A module that declares its own `grammar Grammar` alongside a sibling grammar
# with no explicit `is` parent. In Raku the sibling always inherits the *core*
# `Grammar` Cursor, not this module-local `Grammar`. Mirrors the YAMLish battery
# (`grammar Schema::JSON {}` next to a 780-line `grammar Grammar`).
#
# The main `Grammar` defines its own `method parse` that re-dispatches with an
# Actions class whose `TOP` wraps the result in a 1-list. If a sibling grammar
# wrongly inherits *this* grammar, `Sibling.parse` runs this `parse` + Actions
# and the scalar result is wrapped/lost -- exactly the YAMLish `Schema` bug.

grammar Grammar {
    token TOP { <element> }
    token element { \d+ }

    class Actions {
        method TOP($/) { make ('MAIN-GRAMMAR-WRAPPED',) }
    }

    method parse($string, *%args) {
        nextwith($string, :actions(Actions), |%args);
    }
}

grammar Schema {
    token TOP { <element> { make $<element>.ast } }
    token element { \d+ { make $/.Str.Int } }
}

our sub schema-parse($s) is export { Schema.parse($s).ast }
our sub schema-mro() is export { Schema.^mro.map(*.^name).List }
our sub main-parse($s) is export { Grammar.parse($s).ast }
