use v6;
use Test;

# Chaining and structural (non-associative) comparison operators cannot be
# the base of the `=` assignment metaoperator: `$x OP= $y` desugars to
# `$x = $x OP $y`, which only makes sense when `OP` combines exactly two
# operands into one result. rakudo rejects the metaop with
# `X::Syntax::CannotMeta` ("... because chaining operators are too diffy" /
# "... because structural infix operators are too diffy"), verified against
# `raku -e '6 >== 2'` and friends -- see roast/S03-operators/assign.t
# ("Can't use diffy >= with the = metaop") and
# todo/tickets/vendor-real-test-module.md.

sub cannot-meta-assign(&code, $desc, $operator, $dba) {
    my $ex;
    try {
        code();
        CATCH { default { $ex = $_; } }
    }
    isa-ok $ex, X::Syntax::CannotMeta, "$desc: right exception type";
    is $ex.^name eq 'X::Syntax::CannotMeta' ?? $ex.operator !! Nil,
        $operator, "$desc: .operator";
    is $ex.^name eq 'X::Syntax::CannotMeta' ?? $ex.dba !! Nil,
        $dba, "$desc: .dba";
    is $ex.^name eq 'X::Syntax::CannotMeta' ?? $ex.message !! Nil,
        "Cannot make assignment out of $operator because $dba operators are too diffy",
        "$desc: .message";
}

# Chaining comparison operators.
cannot-meta-assign { EVAL '6 >== 2' }, 'numeric >=', '>=', 'chaining';
cannot-meta-assign { EVAL '6 ==== 2' }, 'identity ===', '===', 'chaining';
cannot-meta-assign { EVAL '6 eq= 2' }, 'string eq', 'eq', 'chaining';
cannot-meta-assign { EVAL '6 ne= 2' }, 'string ne', 'ne', 'chaining';
cannot-meta-assign { EVAL '6 before= 2' }, 'before', 'before', 'chaining';
cannot-meta-assign { EVAL '6 after= 2' }, 'after', 'after', 'chaining';
cannot-meta-assign { EVAL '6 eqv= 2' }, 'eqv', 'eqv', 'chaining';
cannot-meta-assign { EVAL '6 ~~= 2' }, 'smartmatch', '~~', 'chaining';
cannot-meta-assign { EVAL '6 !~~= 2' }, 'negated smartmatch', '!~~', 'chaining';

# Structural (non-associative) comparison operators.
cannot-meta-assign { EVAL '6 cmp= 2' }, 'cmp', 'cmp', 'structural infix';
cannot-meta-assign { EVAL '6 leg= 2' }, 'leg', 'leg', 'structural infix';
cannot-meta-assign { EVAL '6 <=>= 2' }, 'spaceship', '<=>', 'structural infix';

# Range operators are structural too.
cannot-meta-assign { EVAL '6 ..= 2' }, 'range ..', '..', 'structural infix';
cannot-meta-assign { EVAL '6 ..^= 2' }, 'range ..^', '..^', 'structural infix';
cannot-meta-assign { EVAL '6 ^..= 2' }, 'range ^..', '^..', 'structural infix';
cannot-meta-assign { EVAL '6 ^..^= 2' }, 'range ^..^', '^..^', 'structural infix';

# `>=`, `~~`, `<=>`, `..` still work fine on their own -- only the `=`
# metaop over them is rejected.
ok (6 >= 2), 'plain >= still works';
ok !(6 ~~ Str), 'plain ~~ still works';
is (1 <=> 2), Less, 'plain <=> still works';
is (1..5).elems, 5, 'plain .. still works';

# `===`/`!==` are themselves distinct operators (identity / negated
# identity), not `==`/`!=` followed by the `=` metaop, so they must NOT
# raise CannotMeta.
ok !(6 === 2), '=== is a plain operator, not a rejected metaop';
ok (6 !== 2), '!== is a plain operator, not a rejected metaop';

done-testing;
