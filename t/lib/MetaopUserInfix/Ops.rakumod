# The operators a distribution (Red) exports: a `multi infix:<X>` / `infix:<Z>`
# whose `+sources` candidate only accepts operands of its own type.
unit module MetaopUserInfix::Ops;

class Table is export { has $.name }

multi infix:<X>(+sources where { sources.elems >= 2 && sources.grep(Table).elems }) is export {
    "cross of {sources.elems}: " ~ sources.map(*.name).join("+")
}

multi infix:<Z>(+sources where { sources.elems >= 2 && sources.grep(Table).elems }) is export {
    "zip of {sources.elems}: " ~ sources.map(*.name).join("+")
}
