use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST coverage for the constructs the `L10N::ZH` test suite exercises
# (issue #8001): every member of that family is a `Q:to/CODE/.AST($lang).EVAL`,
# so a construct missing from either direction of the RakuAST boundary fails the
# whole file even though the interpreter runs the code fine.
#
# Covered here, all measured against rakudo 2026.07:
#   * `Nil` -- a type object written as a bareword, not a literal value;
#   * `self` -- `RakuAST::Term::Self`, a node with no fields at all;
#   * `our` / `state` variable declarations in the *write* direction (the read
#     direction has rendered `scope` since Phase 2 slice 10);
#   * `redo` -- modelled as a bare call, exactly like `last` / `next`;
#   * `CATCH { ... }` -- `RakuAST::Statement::Catch`;
#   * `subset S of T where P` -- `RakuAST::Type::Subset`;
#   * `module` / `package` -- `RakuAST::Module` / `RakuAST::Package`;
#   * `submethod` -- `RakuAST::Submethod`.
#
# Each declaration uses a distinct name because `.AST` registers the symbol and
# raku rejects redeclaration.
#
# Passes under BOTH mutsu and raku.

plan 24;

# --- `Nil` is a Type::Simple, not a literal ---------------------------------
is Q[Nil].AST.gist, q:to/END/.chomp, 'Nil -> Type::Simple';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Type::Simple.new(
          RakuAST::Name.from-identifier("Nil")
        )
      )
    )
    END

is EVAL(Q[defined(Nil) ?? "d" !! "u"].AST), 'u', 'Nil lowers back to the type object';

# --- `self` ------------------------------------------------------------------
# `self` has to be measured inside a method: raku refuses to compile a bare
# `self` at all ("used where no object is available"), so there is no top-level
# spelling of it to render.
is Q[class S0 { method m() { self } }].AST.gist.lines.grep(*.contains('Term::Self')).head.trim,
    'expression => RakuAST::Term::Self.new',
    'self -> Term::Self, rendered without empty parens';

my $self-node = Q[class S00 { method m() { self } }].AST.statements.head.expression
                  .body.body.statement-list.statements.head.expression
                  .body.statement-list.statements.head.expression;
is $self-node.^name, 'RakuAST::Term::Self', 'the method body is a Term::Self node';
ok $self-node ~~ RakuAST::Term, 'Term::Self is a RakuAST::Term';
ok $self-node ~~ RakuAST::Expression, 'Term::Self is a RakuAST::Expression';

is EVAL(Q[class S1 { method who() { self.^name } }; S1.new.who].AST), 'S1',
    'a method body using self round-trips';

# --- `our` / `state` declarations lower -------------------------------------
is EVAL(Q[our $o1 = 6; $o1].AST), 6, 'an our-scoped declaration lowers';
is EVAL(Q[sub c1() { state $n = 0; ++$n }; c1(); c1(); c1()].AST), 3,
    'a state-scoped declaration lowers and keeps its state across calls';

# --- `redo` ------------------------------------------------------------------
is Q[redo].AST.gist, q:to/END/.chomp, 'redo -> a bare call, like last/next';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name::WithoutParentheses.new(
          name => RakuAST::Name.from-identifier("redo")
        )
      )
    )
    END

is EVAL(Q[my $r = ""; my $d = False; for 1..3 -> $i { if $i == 2 && !$d { $r ~= "X"; $d = True; redo }; $r ~= $i }; $r].AST),
    '1X23', 'redo lowers and re-runs the loop body';

# --- `CATCH { ... }` ---------------------------------------------------------
is Q[CATCH { default { 1 } }].AST.gist, q:to/END/.chomp, 'CATCH -> Statement::Catch with an exception topic block';
    RakuAST::StatementList.new(
      RakuAST::Statement::Catch.new(
        body => RakuAST::Block.new(
          implicit-topic => True,
          required-topic => 1,
          exception      => 1,
          body           => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Default.new(
                body => RakuAST::Block.new(
                  body => RakuAST::Blockoid.new(
                    RakuAST::StatementList.new(
                      RakuAST::Statement::Expression.new(
                        expression => RakuAST::IntLiteral.new(1)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    END

is EVAL(Q[my $c; try { die "boom"; CATCH { default { $c = "caught" } } }; $c].AST),
    'caught', 'CATCH lowers and handles the exception';

# --- `subset` ----------------------------------------------------------------
is Q[subset P1 of Int where * > 0].AST.gist, q:to/END/.chomp, 'subset -> Type::Subset with a Trait::Of base';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Type::Subset.new(
          name   => RakuAST::Name.from-identifier("P1"),
          where  => RakuAST::ApplyInfix.new(
            left  => RakuAST::WhateverCode::Argument.new,
            infix => RakuAST::Infix.new(">"),
            right => RakuAST::IntLiteral.new(0)
          ),
          traits => (
            RakuAST::Trait::Of.new(
              RakuAST::Type::Simple.new(
                RakuAST::Name.from-identifier("Int")
              )
            ),
          )
        )
      )
    )
    END

is EVAL(Q[subset P2 of Int where * > 0; P2.^name].AST), 'P2', 'subset lowers to a live type';
ok EVAL(Q[subset P3 of Int where * > 0; 5 ~~ P3].AST), 'a lowered subset accepts a matching value';
nok EVAL(Q[subset P4 of Int where * > 0; -5 ~~ P4].AST), 'a lowered subset rejects a non-matching value';

# --- `module` / `package` ----------------------------------------------------
# raku gives each declarator keyword its own class rather than one node with a
# `kind` field, and a later bareword naming the package renders as a
# `Type::Simple` just like a class name does.
is Q[package K1 { }].AST.gist, q:to/END/.chomp, 'package -> RakuAST::Package';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Package.new(
          name => RakuAST::Name.from-identifier("K1"),
          body => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new()
            )
          )
        )
      )
    )
    END

is Q[module K2 { }].AST.gist, q:to/END/.chomp, 'module -> RakuAST::Module';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Module.new(
          name => RakuAST::Name.from-identifier("K2"),
          body => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new()
            )
          )
        )
      )
    )
    END

like EVAL(Q[module K3 { }; K3.HOW.^name].AST), rx/ModuleHOW/, 'module lowers to a live module';
like EVAL(Q[package K4 { }; K4.HOW.^name].AST), rx/PackageHOW/, 'package lowers to a live package';

# --- `submethod` -------------------------------------------------------------
is Q[class S2 { submethod BUILD($x) { 1 } }].AST.gist, q:to/END/.chomp, 'submethod -> RakuAST::Submethod, same shape as a Method';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Class.new(
          name => RakuAST::Name.from-identifier("S2"),
          body => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::Submethod.new(
                    name      => RakuAST::Name.from-identifier("BUILD"),
                    signature => RakuAST::Signature.new(
                      parameters => (
                        RakuAST::Parameter.new(
                          type     => RakuAST::Type::Setting.new(
                            RakuAST::Name.from-identifier("Any")
                          ),
                          target   => RakuAST::ParameterTarget::Var.new(
                            name => "\$x"
                          ),
                          optional => False
                        ),
                      )
                    ),
                    body      => RakuAST::Blockoid.new(
                      RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                          expression => RakuAST::IntLiteral.new(1)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    END

is EVAL(Q[class S3 { submethod greet() { "hi" }; method call() { self.greet } }; S3.new.call].AST),
    'hi', 'submethod lowers and stays callable';

# A submethod is not inherited, which is what distinguishes it from a method —
# so the lowered declaration has to keep the `submethod` declarator, not quietly
# become a `method`.
nok EVAL(Q[class S4 { submethod only-mine() { 1 } }; class S5 is S4 { }; S5.^can('only-mine') ?? True !! False].AST),
    'a lowered submethod is not inherited by a subclass';
