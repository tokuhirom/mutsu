use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a heredoc whose body has
# the shape of a string written before it on its line keeps its own ranges;
# the string's do not pass to its body.

plan 2;
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

my $indented = Q:to/SRC/;
my @l = <a b>;
say qq:to/E/, "@l[] @l[0]\n";
    @l[] @l[0]
    E
SRC

my $flush = Q:to/SRC/;
my $x = 1;
say qq:to/E/, "$x\n";
$x
E
SRC

is placed($indented),
    'VarDeclaration::Simple@0-13 Initializer::Assign@6-13 QuotedString@8-13 StrLiteral@9-12 Call::Name::WithoutParentheses@15-43 Name@15-18 ArgList@19-43 Heredoc@19-27 StrLiteral@45-49 ApplyPostfix@49-53 Var::Lexical@49-51 Postcircumfix::ArrayIndex@51-53 SemiList@52-52 StrLiteral@53-54 ApplyPostfix@54-59 Var::Lexical@54-56 Postcircumfix::ArrayIndex@56-59 SemiList@57-58 IntLiteral@57-58 StrLiteral@59-60 QuotedString@29-43 ApplyPostfix@30-34 Var::Lexical@30-32 Postcircumfix::ArrayIndex@32-34 SemiList@33-33 StrLiteral@34-35 ApplyPostfix@35-40 Var::Lexical@35-37 Postcircumfix::ArrayIndex@37-40 SemiList@38-39 IntLiteral@38-39 StrLiteral@40-42',
    'an indented heredoc beside a string of its shape';
is placed($flush),
    'VarDeclaration::Simple@0-9 Initializer::Assign@6-9 IntLiteral@8-9 Call::Name::WithoutParentheses@11-31 Name@11-14 ArgList@15-31 Heredoc@15-23 Var::Lexical@33-35 StrLiteral@35-36 QuotedString@25-31 Var::Lexical@26-28 StrLiteral@28-30',
    'a heredoc beside a string of its shape';
