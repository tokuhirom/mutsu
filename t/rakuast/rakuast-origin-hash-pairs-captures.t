use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a literal-keyed pair in a
# hash composer (`{"1" => 2}`, `{1 => 2}`) places its key, its `=>` and the
# pair, and a capture literal `\(ARGS)` places its `ArgList` over the
# arguments (empty just inside the `(` when there are none).

plan 4;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class ~~ /Statement|Var|Initializer/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[my $h = {"1" => 2};]),
    'Circumfix::HashComposer@8-18 ApplyInfix@9-17 Infix@13-15 ArgList@9-17 QuotedString@9-12 StrLiteral@10-11 IntLiteral@16-17',
    'a quoted-key pair in a hash composer';
is placed(Q[my $h = {1 => 2, 'b' => 3};]),
    'Circumfix::HashComposer@8-26 ApplyListInfix@9-25 Infix@15-16 ApplyInfix@9-15 Infix@11-13 ArgList@9-15 IntLiteral@9-10 IntLiteral@14-15 ApplyInfix@17-25 Infix@21-23 ArgList@17-25 QuotedString@17-20 StrLiteral@18-19 IntLiteral@24-25',
    'number and single-quoted keys';
is placed(Q[say \(1, :a);]),
    'Call::Name::WithoutParentheses@0-12 Name@0-3 ArgList@4-12 Term::Capture@4-12 ArgList@6-11 IntLiteral@6-7 ColonPair::True@9-11',
    'a capture literal with arguments';
is placed(Q[my $c = \();]),
    'Term::Capture@8-11 ArgList@10-10',
    'an empty capture literal';
