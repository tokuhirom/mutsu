use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a quoted method call
# (`$x."uc"()`, `self!"p"(1)`) starts at its name's opening quote; its
# `QuotedString` is the quoted text, and its `ArgList` the inside of the
# parentheses, empty at the `)` when there are no arguments.

plan 6;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) if $class ~~ /Quoted|ArgList|StrLiteral|IntLiteral|Var/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[my $v; $v."uc"()]),
    'VarDeclaration::Simple@0-5 Var::Lexical@7-9 Call::QuotedMethod@10-16 QuotedString@10-14 StrLiteral@11-13 ArgList@15-15',
    'a double-quoted name';
is placed(Q[my $v; $v.'lc'()]),
    'VarDeclaration::Simple@0-5 Var::Lexical@7-9 Call::QuotedMethod@10-16 QuotedString@10-14 StrLiteral@11-13 ArgList@15-15',
    'a single-quoted name';
is placed(Q[my $v; $v."substr"(1, 2)]),
    'VarDeclaration::Simple@0-5 Var::Lexical@7-9 Call::QuotedMethod@10-24 QuotedString@10-18 StrLiteral@11-17 ArgList@19-23 IntLiteral@19-20 IntLiteral@22-23',
    'arguments';
is placed(Q[my $v; my $n; $v."$n"()]),
    'VarDeclaration::Simple@0-5 VarDeclaration::Simple@7-12 Var::Lexical@14-16 Call::QuotedMethod@17-23 QuotedString@17-21 Var::Lexical@18-20 ArgList@22-22',
    'an interpolated name';
is placed(Q[my $v; $v.?"uc"()]),
    'VarDeclaration::Simple@0-5 Var::Lexical@7-9 Call::QuotedMethod@11-17 QuotedString@11-15 StrLiteral@12-14 ArgList@16-16',
    'a .? dispatch';
is placed(Q[class C { method m { self!"p"(1) }; method !p($) { } }]),
    'Call::QuotedMethod@26-32 QuotedString@26-29 StrLiteral@27-28 ArgList@30-31 IntLiteral@30-31 ParameterTarget::Var@46-47 VarDeclaration::Anonymous@46-47',
    'a private quoted call';
