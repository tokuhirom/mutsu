use v6;
use Test;

# RakuAST Phase 2 slice 16 (ADR-0011): role declarations.
# `role NAME { body }` -> RakuAST::Role(name, body => RoleBody(body => Blockoid)).
# Unlike a class, the body is wrapped in a RoleBody (not a plain Block).
# Expected gists captured verbatim from Rakudo; this file passes under BOTH
# mutsu and raku. Distinct role names are used because `.AST` registers the
# symbol.

plan 3;

# --- empty role -------------------------------------------------------------
is Q[role R1 { }].AST.gist, q:to/END/.chomp, 'role -> Role(name, body => RoleBody)';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Role.new(
          name => RakuAST::Name.from-identifier("R1"),
          body => RakuAST::RoleBody.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new()
            )
          )
        )
      )
    )
    END

# --- role with a method -----------------------------------------------------
my $g = Q[role R2 { method m { 1 } }].AST.gist;
ok $g.contains('expression => RakuAST::Role.new(')
    && $g.contains('body => RakuAST::RoleBody.new(')
    && $g.contains('expression => RakuAST::Method.new('),
    'method in role body -> Method inside a RoleBody';

# --- a role body is a RoleBody, not a Block ---------------------------------
is Q[role R3 { }].AST.gist.contains('RakuAST::RoleBody.new('), True,
    'role body node is RoleBody';
