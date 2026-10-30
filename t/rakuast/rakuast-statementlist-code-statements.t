use Test;

plan 5;

# `StatementList.code-statements` (undocumented in rakudo; static::CLI's
# analysis calls it, #15608) answers the same statements as `.statements`
# for every statement kind measured under raku. Read from a CHECK-time
# closure, where the unit's statements exist.
my @seen;
BEGIN {
    my $cu = $*CU;
    $cu.add-check-code({
        my $list = $cu.statement-list;
        @seen = $list.statements.elems, $list.code-statements.elems,
                $list.code-statements.map(*.^name).unique.sort.join(','),
                ($list.code-statements.map(*.fileline) eqv $list.statements.map(*.fileline)),
                $list.code-statements.^name;
    });
}
my $x = 1;
sub foo { 2 }
class C { }
if $x { my $y = 3 }
for 1..2 { my $z = $_ }

is @seen[0], @seen[1], 'same number of statements as .statements';
ok @seen[1] >= 6, 'every top-level statement is listed';
like @seen[2], /'RakuAST::Statement::Expression'/, 'expression statements are listed';
ok @seen[3], 'the statements are the same nodes in the same order';
is @seen[4], 'List', 'it is a List';
