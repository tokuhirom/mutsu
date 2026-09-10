use v6;
use Test;

# RakuAST Phase 2 slice 17 (ADR-0011): loop labels.
# A `LABEL: while/for/loop` prepends a `labels => (Label(name => "..."),)`
# field. Expected gists captured verbatim from Rakudo; this file passes under
# BOTH mutsu and raku.
#
# Note: mutsu stores the label inline on `while`/`for`/bare-`loop`, but wraps a
# labelled `repeat` (and C-style `loop`) in a separate `Stmt::Label` node it
# does not yet parse/convert, so those remain the coverage boundary.

plan 3;

# --- labelled while ---------------------------------------------------------
is Q[LABEL: while 1 { 2 }].AST.gist, q:to/END/.chomp, 'labelled while -> labels first';
    RakuAST::StatementList.new(
      RakuAST::Statement::Loop::While.new(
        labels    => (
          RakuAST::Label.new(
            name => "LABEL"
          ),
        ),
        condition => RakuAST::IntLiteral.new(1),
        body      => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::IntLiteral.new(2)
              )
            )
          )
        )
      )
    )
    END

# --- labelled for: labels before mode ---------------------------------------
my $for = Q[my @a; LOOP: for @a { 1 }].AST.gist;
ok $for.contains('RakuAST::Statement::For.new(')
    && $for.contains('labels => (')
    && $for.contains('RakuAST::Label.new(')
    && $for.contains('name => "LOOP"')
    && $for.index('labels') < $for.index('mode'),
    'labelled for -> labels before mode';

# --- labelled bare loop -----------------------------------------------------
is Q[L: loop { 1 }].AST.gist.contains('name => "L"'), True,
    'labelled bare loop carries its Label';
