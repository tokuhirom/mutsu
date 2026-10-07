use Test;
use lib 't/lib';

# `use` / `no` statements and bare blocks across the RakuAST boundary
# (ADR-10723 Stage 0). Shapes measured against rakudo 2026.09; this file
# passes under both mutsu and raku.

plan 26;

# Read direction: the three node kinds a `use` statement can be.
{
    my @s = Q[use Test; use Test :DEFAULT, :ALL; use lib "lib"; no worries; use MONKEY-SEE-NO-EVAL; use experimental :pack;].AST.statements;
    isa-ok @s[0], RakuAST::Statement::Use, 'use Test is a Statement::Use';
    isa-ok @s[0].module-name, RakuAST::Name, 'its module-name is a Name';
    isa-ok @s[1].argument, RakuAST::ApplyListInfix, 'several import tags are a comma list';
    is @s[1].argument.operands.map(*.key).join(','), 'DEFAULT,ALL', 'one ColonPair::True per tag';
    isa-ok @s[2], RakuAST::Pragma, 'use lib is a Pragma';
    is @s[2].name, 'lib', 'its name';
    isa-ok @s[2].argument, RakuAST::QuotedString, 'its argument is the path expression';
    isa-ok @s[3], RakuAST::Pragma, 'no worries is a Pragma';
    ok @s[3].off, 'switched off';
    nok @s[4].off, 'use MONKEY-SEE-NO-EVAL is not switched off';
    isa-ok @s[5], RakuAST::Statement::Use, 'use experimental is a Statement::Use, not a Pragma';
    isa-ok @s[5].argument, RakuAST::ColonPair::True, 'a single tag is a bare ColonPair::True';
}

{
    my $ast = Q[use v6.d; say 1].AST;
    isa-ok $ast.statements[0], RakuAST::Statement::LanguageVersion, 'use v6.d is a LanguageVersion';
    is $ast.statements[0].version, v6.d, 'its version';
    like $ast.statements[0].gist, /'LanguageVersion.new(v6.d)'/, 'renders the version literal';
}

{
    my $ast = Q[use Test :ALL].AST;
    like $ast.gist, /'module-name => RakuAST::Name.from-identifier("Test")'/, 'module-name renders as a Name';
    like $ast.gist, /'argument    => RakuAST::ColonPair::True.new("ALL")'/, 'a tag renders as ColonPair::True';
}

# Write direction: a converted program runs the same as the source.
is EVAL(Q[use Test; my $r = (ok 1, "inner"); $r].AST), True, 'use Test round-trips and its routines are callable';

{
    my $ast = Q[use RakuASTExportedType; ExportedType].AST;
    isa-ok $ast.statements[1].expression, RakuAST::Type::Simple,
        'plain .AST scans bundled module exports';
    is EVAL($ast).^name, 'RakuASTExportedType::ExportedType',
        'an exported type round-trips through .AST';
    isa-ok Q[use RakuASTExportedType; ExportedType].AST(:compunit)
        .statement-list.statements[1].expression,
        RakuAST::Type::Simple, '.AST(:compunit) scans module exports too';
}

{
    my $out = '';
    EVAL Q[{ $out ~= "ran" }].AST;
    is $out, 'ran', 'a bare block in statement position runs';
}

{
    my $out = '';
    EVAL Q[{ $out ~= "a" }; { $out ~= "b" }].AST;
    is $out, 'ab', 'consecutive bare blocks each run once';
}

{
    my $out = '';
    EVAL Q[my $c = { $out ~= $^x }; $c("p")].AST;
    is $out, 'p', 'a placeholder block is still a closure value';
}

is EVAL(Q[no worries; use MONKEY-SEE-NO-EVAL; EVAL "40 + 2"].AST), 42, 'pragmas round-trip';
