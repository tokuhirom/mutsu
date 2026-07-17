use v6;
use Test;

# RakuAST Phase 1 (ADR-0010): read-only `.AST` introspection for the literal +
# say-call cluster. Expected gists are captured verbatim from Rakudo's
# `Q[...].AST.gist`.

plan 10;

# --- .^name -----------------------------------------------------------------
is Q[42].AST.^name, 'RakuAST::StatementList', 'top-level .AST is a StatementList';

# --- integer literal --------------------------------------------------------
is Q[42].AST.gist, q:to/END/.chomp, 'IntLiteral';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::IntLiteral.new(42)
  )
)
END

# --- rational literal -------------------------------------------------------
is Q[3.5].AST.gist, q:to/END/.chomp, 'RatLiteral';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::RatLiteral.new(3.5)
  )
)
END

# --- string literal (double-quoted) -> QuotedString/StrLiteral --------------
is Q["hi"].AST.gist, q:to/END/.chomp, 'QuotedString + StrLiteral';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedString.new(
      segments   => (
        RakuAST::StrLiteral.new("hi"),
      )
    )
  )
)
END

# --- single-quoted string is also a QuotedString ----------------------------
is Q[q{ab}].AST.gist, q:to/END/.chomp, 'single-quoted string is a QuotedString';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedString.new(
      segments   => (
        RakuAST::StrLiteral.new("ab"),
      )
    )
  )
)
END

# --- listop call (no parens) -> Call::Name::WithoutParentheses --------------
is Q[say 42].AST.gist, q:to/END/.chomp, 'say listop -> WithoutParentheses';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::Call::Name::WithoutParentheses.new(
      name => RakuAST::Name.from-identifier("say"),
      args => RakuAST::ArgList.new(
        RakuAST::IntLiteral.new(42)
      )
    )
  )
)
END

# --- parenthesised call -> Call::Name ---------------------------------------
is Q[say("a")].AST.gist, q:to/END/.chomp, 'say(...) -> Call::Name';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::Call::Name.new(
      name => RakuAST::Name.from-identifier("say"),
      args => RakuAST::ArgList.new(
        RakuAST::QuotedString.new(
          segments   => (
            RakuAST::StrLiteral.new("a"),
          )
        )
      )
    )
  )
)
END

# --- multiple statements ----------------------------------------------------
is Q[say 1; say 2].AST.gist, q:to/END/.chomp, 'two statements, comma-separated';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::Call::Name::WithoutParentheses.new(
      name => RakuAST::Name.from-identifier("say"),
      args => RakuAST::ArgList.new(
        RakuAST::IntLiteral.new(1)
      )
    )
  ),
  RakuAST::Statement::Expression.new(
    expression => RakuAST::Call::Name::WithoutParentheses.new(
      name => RakuAST::Name.from-identifier("say"),
      args => RakuAST::ArgList.new(
        RakuAST::IntLiteral.new(2)
      )
    )
  )
)
END

# --- put/note map to their own names ----------------------------------------
is Q[put 7].AST.gist, q:to/END/.chomp, 'put listop';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::Call::Name::WithoutParentheses.new(
      name => RakuAST::Name.from-identifier("put"),
      args => RakuAST::ArgList.new(
        RakuAST::IntLiteral.new(7)
      )
    )
  )
)
END

# --- string escaping in StrLiteral ------------------------------------------
is Q[say 'a$b'].AST.gist.contains('RakuAST::StrLiteral.new("a\$b")'), True,
    'interpolation sigil is escaped in StrLiteral';
