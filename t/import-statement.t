use Test;
plan 5;

{
    module A {
        sub a() is export { 41 }
        constant c is export = 10;
    }
    import A;
    is a(), 41, 'import brings exported subs into scope';
    is c, 10, 'import brings exported constants into scope';
}

{
    module E {
        sub e1() is export(:A) { 'A' }
        sub e2() is export(:B) { 'B' }
    }
    import E :B;
    dies-ok { EVAL 'e1()' }, 'tagged import does not import other tags';
    is e2(), 'B', 'tagged import imports selected symbol';
}

throws-like 'module H { my $x is export = 1 }', X::Comp::Trait::Scope,
    'my-scoped variables cannot use is export';
