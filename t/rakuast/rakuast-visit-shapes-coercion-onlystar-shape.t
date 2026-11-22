use experimental :rakuast;
use Test;

# Visit shapes measured on rakudo 2026.11 (#18598): a coercion type as a
# term, the implicit `Type::Setting(Any)` constraint of `Rat()`, the empty
# `StatementList` an `OnlyStar` body visits, and `;` sections in a colonpair
# value's parentheses (`:shape(2;2)`), which hold one element each.

plan 9;

sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class ~~ /Statement/ }
        else { @l.push($class) }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}

is placed(Q[say Rat()]),
    'Call::Name::WithoutParentheses@0-9 Name@0-3 ArgList@4-9 Type::Coercion@4-9 Type::Simple@4-9 Name@4-7 Type::Setting Name',
    'Rat() visits its implicit constraint';
is placed(Q[say Int(Str)]),
    'Call::Name::WithoutParentheses@0-12 Name@0-3 ArgList@4-12 Type::Coercion@4-12 Type::Simple@4-12 Name@4-7 Type::Simple@8-11 Name@8-11',
    'a coercion term places its base and constraint';
is placed(Q[my proto sub a($, $?) { * }]),
    'Sub@0-27 Name@13-14 Signature@15-20 Parameter@15-16 Type::Setting Name ParameterTarget::Var@15-16 VarDeclaration::Anonymous@15-16 Parameter@18-20 Type::Setting Name ParameterTarget::Var@18-19 VarDeclaration::Anonymous@18-19 OnlyStar@22-27 StatementList',
    'an OnlyStar body visits an empty statement list';
is placed(Q[Array.new(:shape(2;2))]),
    'ApplyPostfix@0-22 Type::Simple@0-5 Name@0-5 Call::Method@6-22 Name@6-9 ArgList@10-21 ColonPair::Value@10-21 Circumfix::Parentheses@16-21 SemiList@17-20 IntLiteral@17-18 IntLiteral@19-20',
    'a colonpair value has one statement per ; section';

sub f(:$s) { $s.raku }
is f(:s(1,2;3)), '$((1, 2), 3)', 'a multi-item section is a sub-list';
is f(:s(2;2)), '$(2, 2)', 'single-item sections are the items';
is f(:s(1;)), '1', 'a lone section is not wrapped';
is EVAL(Q[sub g(:$s) { $s.raku }; g(:s(1,2;3))].AST), '$((1, 2), 3)', 'EVAL of the tree agrees';
is Array.new(:shape(2;2)).shape.raku, '(2, 2)', 'a shape written with ; sections';
