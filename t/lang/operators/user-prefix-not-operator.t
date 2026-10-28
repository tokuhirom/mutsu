use Test;
use lib $*PROGRAM.parent(3).add('lib');

plan 10;

# `not` is a loose-unary builtin, but a declared or imported `prefix:<not>`
# multi replaces it for its own argument types: `not $x` is that operator's
# call, as `!$x` is for `prefix:<!>` (Red's `multi prefix:<not>(Red::AST $a)`).

{
    class Foo { }
    multi prefix:<not>(Foo $a) { "user-not" }
    multi prefix:<!>(Foo $a) { "user-bang" }
    my $f = Foo.new;

    is (not $f), "user-not", 'a declared prefix:<not> takes `not $x`';
    is (not ($f)), "user-not", '... with a parenthesised operand';
    is (not Foo.new), "user-not", '... with a constructed operand';
    is (!$f), "user-bang", 'prefix:<!> still works beside it';
    sub g($x) { not $x }
    is g($f), "user-not", 'inside a routine';
    is (not 5), False, 'other types keep the builtin not';
    is (not (5)), False, '... also with parentheses';
    is-deeply (not Foo.new, "x"), ("user-not", "x"), 'the operand is a term, not a list';
}

{
    use UserPrefixNot;
    is (not UserPrefixNot::Thing.new), "imported-not", 'an imported prefix:<not> takes `not $x`';
    is (not 0), True, '... and the builtin remains for other types';
}
