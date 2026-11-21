use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a lookaround over a plain
# named assertion (`<?alpha>`, `<!ws>`) places the assertion and its `Name`
# over the name, and the word boundary `<|w>` is placed without a `Name`
# range; the atoms after them are placed too.

plan 4;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) if $class ~~ /Regex|Name/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[/<?alpha> a/]),
    'QuotedRegex@0-12 Regex::Sequence@1-11 Regex::WithWhitespace@1-10 Regex::Assertion::Lookahead@1-9 Regex::Assertion::Named@3-8 Name@3-8 Regex::Literal@10-11',
    'a positive lookaround';
is placed(Q[/<!ws>/]),
    'QuotedRegex@0-7 Regex::Assertion::Lookahead@1-6 Regex::Assertion::Named@3-5 Name@3-5',
    'a negated lookaround';
is placed(Q[/<|w>A<|w>/]),
    'QuotedRegex@0-11 Regex::Sequence@1-10 Regex::Assertion::Named@1-5 Regex::Literal@5-6 Regex::Assertion::Named@6-10',
    'word boundaries';
is placed(Q[/<!|w> b/]),
    'QuotedRegex@0-9 Regex::Sequence@1-8 Regex::WithWhitespace@1-7 Regex::Assertion::Lookahead@1-6 Regex::Literal@7-8',
    'a negated word boundary';
