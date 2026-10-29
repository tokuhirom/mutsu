use Test;
use MONKEY-SEE-NO-EVAL;

# `<thing=@(EXPR)>` / `<x=$(EXPR)>`: an alias over an interpolated expression
# must survive the RakuAST round trip, so a grammar using it loads
# (MUGS::Games, MUGS::Server::Game::Adventure::CommandParser).

plan 5;

my $src = Q|grammar RtAliasInterp {
    rule TOP { ^ <command> $ }
    proto rule command {*}
    rule command:sym<lock> { <sym> <thing=@($*ctx.items)> }
    rule command:sym<look> { <sym> }
    token other { <x=$(~$*ctx)> }
}|;

my $ast;
lives-ok { $ast = $src.AST }, 'the grammar converts to RakuAST';
like $ast.gist, /'Assertion::Alias'/, 'the alias is an Assertion::Alias';
lives-ok { EVAL $ast }, 'and the lowered grammar compiles';
ok ::('RtAliasInterp').^can('command'), 'the grammar has its proto rule';
lives-ok { Q|/<x=@(<a b>)>/|.AST }, 'a regex literal with the alias converts too';
