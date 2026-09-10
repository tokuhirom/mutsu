use Test;

# A role parameterised by NAMED parameters never matched, so neither
# `but R[:v(...)]` nor `does R[:v(...)]` worked.
# See https://github.com/tokuhirom/mutsu/issues/7757
plan 10;

role R[Str:D :$v] { method x { $v } };

# --- the repro ---
{
    class C {};
    my $c = C.new but R[:v<hi>];
    is $c.x, 'hi', 'but R[:v<hi>] binds the named role parameter';
}

# --- the does spelling, with no sink-context warning ---
{
    class D does R[:v<hi>] {};
    is D.new.x, 'hi', 'does R[:v<hi>] binds the named role parameter too';
}

# --- named-only, via a colon-pair-syntax with parens ---
{
    role RP[Str:D :$v] { method x { $v } };
    class E {};
    is (E.new but RP[:v('paren')]).x, 'paren', ':v(...) colonpair form binds too';
}

# --- mixed positional + named role signature ---
{
    role Mixed[Str:D $pos, Int:D :$n] { method y { "$pos-$n" } };
    class M {};
    is (M.new but Mixed["a", :n(3)]).y, 'a-3',
        'mixed positional+named role parameters bind correctly';
}

# --- controls that were already correct must stay correct ---
{
    role R2[::T] { method x { T.^name } };
    class C2 {};
    is (C2.new but R2[Int]).x, 'Int', 'positional type-capture role parameter is unaffected';
}
{
    role R3[$v] { method x { $v } };
    class C3 {};
    is (C3.new but R3["hi"]).x, 'hi', 'positional value role parameter is unaffected';
}

# --- an explicit fat-arrow is data, not a named argument (matches Rakudo:
# a Pair VALUE passed positionally does not bind a role's named parameter) ---
{
    class F {};
    dies-ok { F.new but R[ 'v' => 'hi' ] },
        'an explicit fat-arrow Pair does not bind as a named role parameter';
}

# --- a comma list of positional type args is still a single ArrayLiteral
# compile path and must be unaffected by the named-arg detection ---
{
    role NoNamed[$a, $b] { method x { "$a,$b" } };
    class G {};
    is (G.new but NoNamed[1, 2]).x, '1,2', 'a plain positional comma-list role arg list is unaffected';
}

# --- ordinary array/hash subscripts (unrelated to roles) stay unaffected
# by the compiler change to subscript-index compilation ---
{
    my @a = (1, 2, 3);
    is @a[1], 2, 'plain array subscript is unaffected';
    my %h = (a => 1, b => 2);
    is %h<a>, 1, 'plain hash subscript is unaffected';
}

done-testing;
