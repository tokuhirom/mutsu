use Test;

# `callsame`/`callwith`/`nextsame` from a user `multi prefix:<op>` reaches the
# core prefix operator as the implicit final candidate, exactly as it already
# did for `infix:<op>`. It used to answer Nil, so a modular negation
# (`callsame() mod $*modulus`, the FiniteFields dist used by EC) computed 0.

plan 7;

{
    multi prefix:<->(UInt $n) { callsame() mod 7 }
    is -5, 2, 'callsame from prefix:<-> reaches the core negation';
    my $x = 3;
    is -$x, 4, 'also for a variable operand';
}

{
    multi prefix:<~>(Int $n) { '<' ~ callsame() ~ '>' }
    is ~5, '<5>', 'callsame from prefix:<~> reaches the core stringification';
}

{
    multi prefix:<!>(Int $n) { callwith(0) }
    is !5, True, 'callwith from prefix:<!> passes the new argument to the core candidate';
}

{
    multi prefix:<+>(Str $s) { nextsame }
    is +"42", 42, 'nextsame from prefix:<+> reaches the core numification';
}

{
    multi prefix:<?>(Int $n) { callsame() ?? 'yes' !! 'no' }
    is ?0, 'no', 'callsame from prefix:<?> reaches the core boolification';
    is ?1, 'yes', 'and for a true operand';
}
