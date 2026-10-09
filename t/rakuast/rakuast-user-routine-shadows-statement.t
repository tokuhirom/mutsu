use Test;
use lib 't/lib';

# A routine the program declares itself wins over the builtin of the same name,
# because the parser sees the declaration. A tree read back from source has to
# make the same choice: `say`, `put`, `print`, `note`, `die`, `fail` and `take`
# are statements of their own once lowered, which a declared `sub` takes back.

plan 13;

sub run($src) { EVAL($src.AST) }

for <say put print note die fail take> -> $name {
    my $program = 'sub NAME($n) { "mine " ~ $n }; NAME(5)'.subst('NAME', $name, :g);
    is run($program), 'mine 5', "a declared sub $name is called, not the builtin";
}

# A lexical `&name` shadows the builtin too.
is run(Q[my &take = -> $n { "mine $n" }; take(6)]), 'mine 6', 'a lexical &take shadows the builtin';
# Without a declaration the builtin is still the statement.
is run(Q[my @a = gather { take 1; take 2 }; @a.join(',')]), '1,2', 'the builtin take still gathers';

# The parser resolves imported routines before the import's lexical scope is
# discarded. Both the call target and the written parentheses survive lowering.
my $push = Q[use ListopShadow; my @a = 1; push(@a, 42)].AST.statements[*-1].expression;
is $push.^name, 'RakuAST::Call::Name', 'imported push keeps parentheses';
is run(Q[use ListopShadow; my @a = 1; push(@a, 42)]), 2,
    'an imported push keeps its routine result';
my $put = Q[use IOBuiltinShadow; put -> 'x' { 1 }].AST.statements[*-1].expression;
is $put.^name, 'RakuAST::Call::Name::WithoutParentheses',
    'imported put keeps its bare-call spelling';
is run(Q[use IOBuiltinShadow; put -> 'x' { 1 }; shadow-log()]), 'put',
    'an imported put is called rather than printed';
