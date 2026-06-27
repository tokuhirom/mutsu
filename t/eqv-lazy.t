use Test;

plan 16;

# `.List` / `.Array` on a genuinely-lazy sequence preserve laziness (they must
# NOT materialize the infinite tail) and report the coerced `.WHAT`.
{
    ok (1…∞).is-lazy,        'infinite Seq is lazy';
    ok (1…∞).List.is-lazy,   '.List of a lazy Seq stays lazy';
    ok (1…∞).Array.is-lazy,  '.Array of a lazy Seq stays lazy';
    is (1…∞).WHAT.^name,       'Seq',   'lazy Seq .WHAT is Seq';
    is (1…∞).List.WHAT.^name,  'List',  'lazy .List .WHAT is List';
    is (1…∞).Array.WHAT.^name, 'Array', 'lazy .Array .WHAT is Array';
    is-deeply (1…∞).List.head(3).List,  (1, 2, 3), 'lazy .List still iterates';
    is-deeply (1…∞).Array.head(3).List, (1, 2, 3), 'lazy .Array still iterates';
}

# A finite sequence coerces eagerly (not lazy).
{
    nok (1…3).List.is-lazy,  'finite .List is eager';
    nok (1…3).Array.is-lazy, 'finite .Array is eager';
}

# `eqv` on two lazy iterables of the SAME type throws X::Cannot::Lazy; of
# different types (or with only one lazy operand) it lives.
{
    throws-like { (1…∞)       eqv (1…∞)       }, X::Cannot::Lazy, :action<eqv>,
        'both lazy Seqs throw';
    throws-like { (1…∞).List  eqv (1…∞).List  }, X::Cannot::Lazy, :action<eqv>,
        'both lazy Lists throw';
    throws-like { (1…∞).Array eqv (1…∞).Array }, X::Cannot::Lazy, :action<eqv>,
        'both lazy Arrays throw';
    lives-ok { (1…∞) eqv (1…∞).List }, 'both lazy but different type lives';
    lives-ok { (1…∞) eqv (1, 3)     }, 'only one lazy lives';
    lives-ok { (1…∞).List eqv (1…3).List }, 'one lazy List lives';
}
