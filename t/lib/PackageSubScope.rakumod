unit module PackageSubScope;

# A plain `sub` in a module body is `my`-scoped: exported and callable under
# its short name, but NOT a package symbol.
sub lex-sub() is export { "lex" }
multi sub multi-lex(Int $x) is export { "multi-lex $x" }
multi sub multi-lex(Str $x) is export { "multi-lex str $x" }

# `our sub` publishes the name in the package stash.
our sub pkg-sub() is export { "pkg" }
our proto sub multi-pkg(|) is export {*}
multi sub multi-pkg(Int $x) { "multi-pkg $x" }

# Not even from inside the package: a lexical routine is never in the stash,
# so the qualified form does not reach it there either.
our sub inside-qualified-lex() is export {
    PackageSubScope::lex-sub()
}
