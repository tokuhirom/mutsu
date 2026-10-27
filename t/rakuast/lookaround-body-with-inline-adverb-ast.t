use Test;

plan 9;

# A lookaround whose body carries an inline adverb (`<?before [:i 'not(']>`)
# still has a source tree, so a rule using it crosses the `.AST` boundary
# (CSS::Module::CSS3::Selectors' `negation-expr`).
my $n = 0;
for 'rule a { <?before [:i x]> }',
    'rule a { <?before :i x> }',
    'rule a { <!before [:i x]> }',
    'rule a { <?after [:i x]> }',
    'rule a { [$<n>=<?before [:i"not("]> || <?>] <foo> }' -> $decl {
    my $name = "G{++$n}";
    lives-ok { "grammar $name \{ $decl \}".AST }, ".AST of $decl";
}

grammar Neg {
    token TOP { <?before [:i 'not(']> \w+ '(' \w+ ')' }
}
ok Neg.parse('NOT(abc)'), 'the lookahead matches upper case';
ok Neg.parse('not(abc)'), 'and lower case';
is Neg.parse('NoT(x)').Str, 'NoT(x)', 'and mixed case';
nok Neg.parse('xot(abc)'), 'but still rejects other text';
