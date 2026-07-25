unit module ModuleLocalShadow;

# A grammar whose short name collides with the built-in `Grammar` type.
grammar Grammar {
    token TOP { <num> '-' <num> }
    token num { \d+ }
}

# A class whose short name collides with the built-in `Int` type.
class Int {
    has $.tag = "module-local-Int";
}

our sub parse-it(Str $input) is export {
    # Unqualified `Grammar` must resolve to ModuleLocalShadow::Grammar, not core.
    return Grammar.parse($input);
}

our sub grammar-name() is export { Grammar.^name }

our sub make-int() is export {
    # Unqualified `Int` must resolve to ModuleLocalShadow::Int here.
    return Int.new;
}
