use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a substitution places its
# adverbs, its pattern, its replacement (a string between delimiters, or the
# expression after `=` / `OP=` with the operator), and visits its pattern
# before its operator.

plan 10;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class ~~ /Statement/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[s:g/a/b/]),
    'Substitution@0-8 ColonPair::True@1-3 Regex::Literal@4-5 QuotedString@6-7 StrLiteral@6-7',
    'adverbs, pattern and replacement between delimiters';
is placed(Q[s[\d] = 5]),
    'Substitution@0-9 Regex::CharClass::Digit@2-4 Assignment@6-7 IntLiteral@8-9',
    'a bracketed pattern and an assignment';
is placed(Q[s{a}="b"]),
    'Substitution@0-8 Regex::Literal@2-3 Assignment@4-5 QuotedString@6-7',
    'a quoted replacement after `=`';
is placed(Q[s[\d] += 5]),
    'Substitution@0-10 Regex::CharClass::Digit@2-4 MetaInfix::Assign@6-8 Infix@6-7 IntLiteral@9-10',
    'a compound assignment';
is placed(Q[s:i:g{ a } = 'c']),
    'Substitution@0-16 ColonPair::True@1-3 ColonPair::True@3-5 Regex::WithWhitespace@7-9 Regex::Literal@7-8 Assignment@11-12 QuotedString@14-15',
    'adverbs and a blank-padded pattern';
is placed(Q[S/a/$0/]),
    'Substitution@0-7 Regex::Literal@2-3 QuotedString@4-6 Var::PositionalCapture@4-6',
    'an interpolated replacement';
is placed(Q[s!a!b!]),
    'Substitution@0-6 Regex::Literal@2-3 QuotedString@4-5 StrLiteral@4-5',
    'another delimiter';
is placed(Q[ss/a b/c d/]),
    'Substitution@0-11 Regex::Sequence@3-6 Regex::WithWhitespace@3-5 Regex::Literal@3-4 Regex::Literal@5-6 QuotedString@7-10 StrLiteral@7-10',
    'a samespace substitution';

# A lone literal after `=` is the replacement expression, not a string to
# interpolate.
my $x = 1;
my $s = "a";
$s ~~ s{a} = q{$x};
is $s, '$x', 'a single-quoted replacement is not interpolated';
$s = "a b";
$s ~~ ss[a b] = "c d";
is $s, 'c d', 'ss[...] = EXPR';
