use v6;
use Test;

# `grammar Foo is export { ... }` must treat `export` as a trait, not as a parent
# grammar. Previously the grammar declarator grabbed every `is X` as a superclass,
# so `is export` tried to inherit from a nonexistent `export`. Class and role
# declarators already handled this; grammars did not. Surfaced by PostCocoon::Url.

plan 4;

grammar Digits is export {
    token TOP { \d+ }
}
ok Digits.parse("12345"), "grammar with `is export` parses input";
nok Digits.parse("abc"), "grammar with `is export` rejects non-matching input";

# A real parent grammar (uppercase name) is still honored as inheritance.
grammar Base {
    token num { \d+ }
}
grammar Sub is Base {
    token TOP { <num> }
}
ok Sub.parse("42"), "grammar `is Base` still inherits tokens";

# `is export` combined with a real parent, in either order.
grammar Both is Base is export {
    token TOP { <num> }
}
ok Both.parse("7"), "grammar `is Base is export` inherits and exports";
