use v6;
use Test;

# Round 21 of #7576: a `<subrule>` call skips its left-recursion activation
# entirely when the rule call graph proves nothing can re-enter the key — three
# thread-local map operations per call on a grammar with no left recursion.
#
# The gate has exactly two preconditions, and this file pins the constructs that
# have to keep failing them:
#
#   * nothing reachable from the call names a rule of the same name (the
#     growing-seed loop is the answer when one does), and
#   * nothing in the call cone runs USER CODE. A `{ ... }` block, a `<?{ ... }>`
#     assertion, a `:my $x = ...` declaration and a `** { ... }` quantifier bound
#     are all arbitrary code that could call the rule by hand at the same
#     position, which the activation — and only the activation — makes
#     terminate.
#
# A widened gate that admitted any of these would not give a wrong answer; it
# would recurse until the stack ran out. So every row here is also a
# does-it-terminate row.
#
# Every row except the two explicitly-left-recursive ones was verified against
# rakudo 2026.07. Rakudo has no growing-seed loop and hangs on a left-recursive
# rule, the same caveat `t/regex/regex-left-recursion-key-identity.t` records.

plan 12;

# --- the gate must stay shut: a rule that reaches its own name ---------------

grammar LeftRec {
    token TOP  { <expr> }
    token expr { <expr> '+' <term> | <term> }
    token term { \d+ }
}
is ~(LeftRec.parse('1+2+3') // ''), '1+2+3', 'a directly left-recursive rule still grows its seed';

grammar MutualRec {
    token TOP  { <a> }
    token a    { <b> '!' | <lit> }
    token b    { <a> }
    token lit  { 'x' }
}
is ~(MutualRec.parse('x!') // ''), 'x!', 'a MUTUALLY recursive pair still grows its seed';

# --- the gate must stay shut: user code anywhere in the cone ----------------

# A `{ ... }` block re-entering the same rule name at the same position. The
# outer call is at `remaining == 1` and so is the nested parse, so both land on
# the same left-recursion key; the depth guard is what makes the construct
# finite at the Raku level, and the activation is what makes the ENGINE finite.
my $depth = 0;
my $inner = 'not-run';
grammar BlockReenter {
    token TOP   { <thing> }
    token thing {
        'a'
        { if $depth == 0 { $depth = 1; $inner = BlockReenter.parse('a', :rule<thing>).defined.Str } }
    }
}
ok BlockReenter.parse('a').defined, 'a rule whose block re-enters its own name still parses';
is $inner, 'True', '... and the re-entering nested parse succeeds';

grammar CodeAssert {
    token TOP  { <word> }
    token word { \w+ <?{ $/.chars == 3 }> }
}
ok CodeAssert.parse('abc').defined, 'a `<?{ ... }>` assertion in the cone accepts its match';
nok CodeAssert.parse('abcd').defined, '... and rejects the one it should';

grammar QuantCode {
    token TOP { <pad> 'x' }
    token pad { ' ' ** { 1..3 } }
}
ok QuantCode.parse('  x').defined, 'a `** { ... }` quantifier bound in the cone still matches';

my @fired;
grammar BlockSideEffect {
    token TOP   { <thing>+ }
    token thing { $<c>=[\w] { @fired.push(~$<c>) } }
}
ok BlockSideEffect.parse('abc').defined, 'a rule with a side-effecting block parses';
is @fired.join(','), 'a,b,c', '... and its block fires once per matched item, in order';

# --- the gate must OPEN: plain rules, whose results must not change ----------

grammar Plain {
    token TOP     { <entry>+ % ',' }
    token entry   { <key> '=' <value> }
    token key     { <[a..z]>+ }
    token value   { <digits> | <word> }
    token digits  { \d+ }
    token word    { \w+ }
}
my $m = Plain.parse('a=1,bb=xy,c=3');
ok $m.defined, 'a code-free, recursion-free grammar parses';
is $m<entry>.map({ ~.<key> }).join('|'), 'a|bb|c', '... with every capture where it belongs';

# The left-recursion key carries no package, while the gate's analysis is per
# package: a rule name shared by two grammars must still resolve to its own
# rule, and neither call may read the other's seed.
grammar ShareA { token TOP { <part> }; token part { 'x' } }
grammar ShareB { token TOP { <part> }; token part { 'y' } }
ok ShareA.parse('x').defined && ShareB.parse('y').defined && !ShareA.parse('y').defined,
    'a rule name shared by two grammars resolves per grammar';
