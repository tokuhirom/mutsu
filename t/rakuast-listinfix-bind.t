use v6;
use Test;

# RakuAST Phase 2 slice 9 (ADR-0011): comma lists (ApplyListInfix) and `:=`
# binding (a plain `:=` Infix, unlike `=`'s special Assignment node).
# Expected gists captured verbatim from Rakudo; this file passes under BOTH
# mutsu and raku.

plan 4;

# --- bare comma list -> ApplyListInfix --------------------------------------
is Q[1, 2, 3].AST.gist, q:to/END/.chomp, 'comma list -> ApplyListInfix(",", operands)';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyListInfix.new(
          infix    => RakuAST::Infix.new(","),
          operands => (
            RakuAST::IntLiteral.new(1),
            RakuAST::IntLiteral.new(2),
            RakuAST::IntLiteral.new(3),
          )
        )
      )
    )
    END

# --- a two-element list -----------------------------------------------------
is Q["a", "b"].AST.gist.contains(q:to/FRAG/.chomp), True, 'string list operands';
        expression => RakuAST::ApplyListInfix.new(
          infix    => RakuAST::Infix.new(","),
          operands => (
            RakuAST::QuotedString.new(
    FRAG

# --- `:=` binding -> a plain Infix, not Assignment --------------------------
# The whole bind is an ApplyInfix over Infix(":=") with both var operands, and
# crucially NOT the special Assignment node that `=` uses.
my $bind = Q[my $x; my $y; $x := $y].AST.gist;
ok $bind.contains('infix => RakuAST::Infix.new(":=")')
    && $bind.contains('left  => RakuAST::Var::Lexical.new("\$x")')
    && $bind.contains('right => RakuAST::Var::Lexical.new("\$y")')
    && !$bind.contains('Assignment'),
    'scalar bind -> ApplyInfix over Infix(":="), not Assignment';

# --- array binding also uses a plain Infix (no :item, no Assignment) --------
is Q[my @a; @a := 5].AST.gist.contains('infix => RakuAST::Infix.new(":=")'), True,
    'array bind -> Infix(":=")';
