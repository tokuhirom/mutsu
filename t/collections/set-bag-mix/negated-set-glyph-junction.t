use Test;

plan 9;

# The precomposed negated set glyphs are routines of their own over Any, so a
# Junction operand autothreads and the negation applies per eigenstate. The
# `!` meta-prefix instead negates the collapsed Bool.
is (1 ∉ any((1,),(4,))).raku, 'any(Bool::False, Bool::True)', '∉ autothreads';
is (any((1,),(4,)) ∌ 1).raku, 'any(Bool::False, Bool::True)', '∌ autothreads';
is (1 ⊈ any(1,4)).raku, 'any(Bool::False, Bool::True)', '⊈ autothreads';
is (1 ⊄ any(1,4)).raku, 'any(Bool::True, Bool::True)', '⊄ autothreads';
is (1 ⊅ all(1,4)).raku, 'all(Bool::True, Bool::True)', '⊅ autothreads (all)';
ok so(none(1,2) ⊉ any(1,2)) === False, '⊉ keeps the none/any nesting';
is (1 !(elem) any((1,),(4,))).raku, 'Bool::False', '!(elem) meta form collapses';
ok 9 ∉ (1, 2), 'plain operands still give a Bool';
nok 1 ⊄ (1, 2, 3), 'plain ⊄ still works';

done-testing;
