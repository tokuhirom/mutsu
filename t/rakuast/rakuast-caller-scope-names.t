use Test;

# The bare names of a `Str.AST` source and of an `EVAL` string resolve against
# the scope that runs them, measured on rakudo 2026.09: `constant b = 256;
# q[b].AST` renders `b` as a `Term::Name`, and so does a type the caller
# declares (only the source's own and the setting's types are `Type::Simple`).

plan 9;

constant cb = 256;
class CallerType { method v { 7 } }

sub expr($src) { $src.AST.statements[0].expression }

# --- read direction: the caller's names classify the source's barewords
isa-ok expr(q[cb]), RakuAST::Term::Name, 'a caller constant is a Term::Name';
isa-ok expr(q[CallerType]), RakuAST::Term::Name, 'a caller class is a Term::Name too';
{
    isa-ok expr(q[cb]), RakuAST::Term::Name, '... also from a nested block';
}

# --- EVAL of the tree computes what the caller scope holds
is q[cb + 1].AST.EVAL, 257, 'EVAL of the tree reads the caller constant';
is q[CallerType.v].AST.EVAL, 7, 'EVAL of the tree reaches the caller class';

# --- semantics: EVAL of a string sees an outer constant from a nested block,
# also when the unit declares routines after it
{
    is (EVAL 'cb'), 256, 'EVAL of a string in a nested block sees the constant';
    {
        is (EVAL 'cb * 2'), 512, '... two blocks deep';
    }
}
{
    { constant inner-c = 42; }
    is (EVAL 'inner-c'), 42, 'an inner-block constant is visible to a later EVAL';
}

sub declared-after { 1 }
is declared-after(), 1, 'a routine declared after the blocks';
