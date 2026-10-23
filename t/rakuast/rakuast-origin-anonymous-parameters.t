use Test;

# An anonymous variable in a declaration signature (`my ($, $b)`) or a
# routine's signature (`sub f($, @)`) is spelled with its bare sigil, its
# target declares a `VarDeclaration::Anonymous`, and both carry the range of
# the sigil (ADR-10723 Stage 2). Ranges measured on rakudo 2026.09.

plan 7;

# Each visited node of `$src`, with its range.
sub nodes(Str $src) {
    my @r;
    sub walk($node) {
        @r.push: $node.^name.subst('RakuAST::', '') ~ ' '
            ~ ($node.origin andthen "{.from}..{.to}" orelse '-');
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r
}

my $decl = 'my ($, $b) = 1, 2;'.AST.statements[0].expression;
is-deeply $decl.signature.parameters.map(*.target.name).List, ('$', '$b'),
    'an anonymous element is spelled with its bare sigil';

is nodes('my ($, $b) = 1, 2;')[2..7].join(', '),
    'Signature 4..9, Parameter 4..5, ParameterTarget::Var 4..5, '
    ~ 'VarDeclaration::Anonymous 4..5, Parameter 7..9, ParameterTarget::Var 7..9',
    'a declaration signature with an anonymous scalar';

is nodes('my (@, %h) = ();')[2..5].join(', '),
    'Signature 4..9, Parameter 4..5, ParameterTarget::Var 4..5, VarDeclaration::Anonymous 4..5',
    'an anonymous array';

is nodes('sub f($, @) { }')[3..9].join(', '),
    'Signature 6..10, Parameter 6..7, Type::Setting -, Name -, '
    ~ 'ParameterTarget::Var 6..7, VarDeclaration::Anonymous 6..7, Parameter 9..10',
    'anonymous routine parameters';

is nodes('sub g(Int $, *@) { }').grep(/Target|Anonymous/).join(', '),
    'ParameterTarget::Var 10..11, VarDeclaration::Anonymous 10..11, '
    ~ 'ParameterTarget::Var 14..15, VarDeclaration::Anonymous 14..15',
    'a typed and a slurpy anonymous parameter';

my ($x, $y);
EVAL 'my ($, $b) = 1, 2; $y = $b'.AST;
is $y, 2, 'the declaration still runs from its RakuAST';
EVAL Q[sub f($, $z) { $z }; $x = f(1, 3)].AST;
is $x, 3, 'and so does the routine';
