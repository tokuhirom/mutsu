use Test;

plan 5;

# EVAL/throws-like's string form runs a static undeclared-variable pre-check
# over the parsed AST before execution. It walked a Stmt::MethodDecl's
# user-written params only, with no knowledge that a signature-less method
# body legitimately gets an implicit `*%_` (unless the class is `is hidden`
# or the signature already names an explicit named slurpy) or, if it reads
# a bare `@_` directly, an implicit `*@_` that binds any arity before a
# runtime die -- so `%_`/`@_` inside a method body reached through EVAL's
# string form were wrongly flagged X::Undeclared, even though calling the
# identical method directly (not through EVAL) works fine. See
# todo/tickets/eval-undeclared-check-blind-to-implicit-method-slurpy.md.

is EVAL(q[class EvalSlurpyD1 { method m { %_.elems } }; EvalSlurpyD1.new.m(a=>1,b=>2)]), 2,
    'a class method reading the implicit %_ works through EVAL';

is EVAL(q[role EvalSlurpyR2 { method m { %_.elems } }; class EvalSlurpyD2 does EvalSlurpyR2 {}; EvalSlurpyD2.new.m(a=>1,b=>2)]), 2,
    'a role method reading the implicit %_ works through EVAL';

is EVAL(q[class EvalSlurpyD3 { has $.x; method m { %_.elems + $.x } }; EvalSlurpyD3.new(x=>10).m(a=>1,b=>2)]), 12,
    '%_ combined with an attribute read still works through EVAL';

{
    my $is_undeclared;
    try {
        EVAL(q[class EvalSlurpyD4 { method m { @_ } }; EvalSlurpyD4.new.m(1,2,3)]);
        CATCH {
            default { $is_undeclared = $_ ~~ X::Undeclared }
        }
    }
    is $is_undeclared, False,
        'a direct bare @_ read in a method body dies via its own specific placeholder check, not X::Undeclared, through EVAL';
}

# Sanity: EVAL still catches a genuinely undeclared variable in a method body.
throws-like
    q[class EvalSlurpyD5 { method m { $totally_undeclared_var_xyz } }; EvalSlurpyD5.new.m],
    X::Undeclared,
    'EVAL still flags a genuinely undeclared variable inside a method body';
