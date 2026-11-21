use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: an interpolating heredoc's
# segments are placed over its indented body; a unit whose last statement
# opens a heredoc ends its statement list before the body; a quote with
# interpolation adverbs (`Q:s|...|`, `q:a{...}`) places its segments.

plan 4;
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
is placed("my \$n = 1;\nsay qq:to/END/.chomp, 5;\n  a \$n b\n  c\n  END\n"),
    'VarDeclaration::Simple@0-9 Initializer::Assign@6-9 IntLiteral@8-9 Call::Name::WithoutParentheses@11-34 Name@11-14 ArgList@15-34 ApplyPostfix@15-31 Heredoc@15-25 StrLiteral@36-40 Var::Lexical@40-42 StrLiteral@42-49 Call::Method@26-31 Name@26-31 ArgList@31-31 IntLiteral@33-34',
    'an interpolating heredoc, dedented';
is placed("say q:to/X/;\nplain\nX\n"),
    'Call::Name::WithoutParentheses@0-11 Name@0-3 ArgList@4-11 Heredoc@4-11 StrLiteral@13-19',
    'a plain heredoc';
is placed("my \$n = 1; my \@a = 2;\nsay Q:s|a \$n|;\nsay q:a\{x \@a[0]};\n"),
    'VarDeclaration::Simple@0-9 Initializer::Assign@6-9 IntLiteral@8-9 VarDeclaration::Simple@11-20 Initializer::Assign@17-20 IntLiteral@19-20 Call::Name::WithoutParentheses@22-35 Name@22-25 ArgList@26-35 QuotedString@26-35 StrLiteral@30-32 Var::Lexical@32-34 Call::Name::WithoutParentheses@37-53 Name@37-40 ArgList@41-53 QuotedString@41-53 StrLiteral@45-47 ApplyPostfix@47-52 Var::Lexical@47-49 Postcircumfix::ArrayIndex@49-52 SemiList@50-51 IntLiteral@50-51',
    'quotes with interpolation adverbs';

with "say q:to/X/;\nplain\nX\n".AST.origin {
    is "{.from}-{.to}", '0-12', 'the unit list ends before the heredoc body';
}
