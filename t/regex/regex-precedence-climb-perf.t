use Test;

# A precedence-climbing grammar (`multi rule expr($p) { <expr($p-1)> *% [...] }`,
# 99problems-41-to-50.t P47) re-entered every subrule TWICE per nesting level:
# the left-recursion seed-growing loop always ran a second iteration to observe
# "no growth", even for rules that are not left-recursive at that position. That
# redundant pass compounded to 2^depth, so `not B` took ~14s and `not not B` did
# not finish. A rule whose seed is never consulted cannot change when the seed
# grows, so the first result is final.
#
# These assertions pin the RESULT; the plan-level guard is that the file
# completes at all (it used to hang).

plan 8;

grammar Prec {
    rule TOP { <expr(3)> }
    multi rule expr(0)  { <term> }
    multi rule expr($p) { <expr($p-1)> *%[ <?before <pred($p)>> <op> ] }
    multi token pred(3) { equ|impl }
    multi token pred(2) { or|nor|xor }
    multi token pred(1) { and|nand }
    proto token op {*}
    token op:sym<and> {<sym>}
    token op:sym<or>  {<sym>}
    token op:sym<equ> {<sym>}
    proto token term {*}
    token term:sym<var>   { <[A..Z]> }
    rule  term:sym<not>   { <sym> <term=.expr(3)> }
    rule  term:sym<paren> { '(' ~ ')' [ <term=.expr(3)> ] }
}

ok Prec.parse('A'),                   'single term';
ok Prec.parse('A and B'),             'one infix op';
ok Prec.parse('A and B or C'),        'mixed precedence chain';
ok Prec.parse('not B'),               'prefix not (was ~14s before the seed-loop fix)';
ok Prec.parse('not not B'),           'nested not (did not terminate before the fix)';
ok Prec.parse('(A or B)'),            'parenthesised sub-expression';
ok Prec.parse('A and (A or not B)'),  'nested parens + not';
ok Prec.parse('A equ (not A or B) and C'), 'deeper nesting stays tractable';

# vim: expandtab shiftwidth=4
