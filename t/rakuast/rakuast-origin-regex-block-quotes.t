use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a code block inside a regex
# (`<?{ ... }>`, `<!{ ... }>`, `** { ... }`) is placed even when its code holds
# a quoted string, a brace inside one included, and a bare code block
# (`Regex::Block`, `{ say 1 }`) is placed like them.

plan 4;

sub placed($src, *@classes) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) if $class eq any(@classes) }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}

is placed(Q[my $x; my $y = rx{ <?{ $x("a}") }> };], <Block Regex::Assertion::PredicateBlock StrLiteral>),
    'Regex::Assertion::PredicateBlock@19-34 Block@21-33 StrLiteral@27-29',
    'a predicate block with a string argument';
is placed(Q[grammar G { token w { <!{ 'don\'t' eq "x" }> { say "}" } } }], <Block StrLiteral Regex::Block>),
    'Block@10-60 Block@24-43 StrLiteral@27-33 StrLiteral@39-40 Regex::Block@45-56 Block@45-56 StrLiteral@52-53',
    'a negated predicate and a code block with quoted braces';
is placed(Q[my $r = rx{ a ** {"x".chars} };], <Block StrLiteral>),
    'Block@17-28 StrLiteral@19-20', 'a block quantifier with a string';
is placed(Q[grammar H { token w { a { say 1 } } }], <Regex::Block Regex::Sequence StatementList>),
    'StatementList@0-37 StatementList@12-35 Regex::Sequence@22-34 Regex::Block@24-33 StatementList@26-31',
    'a bare code block';
