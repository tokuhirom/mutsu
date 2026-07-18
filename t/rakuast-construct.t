use v6;
use experimental :rakuast;
use Test;

# RakuAST Phase 4 slice 1 (ADR-0011): construction of literal nodes via `.new`.
# `RakuAST::IntLiteral.new(42)` builds a real RakuAST node — renderable (`.gist`)
# and queryable (`.value`, `~~`). Verified against Rakudo; this file passes
# under BOTH mutsu and raku.

plan 6;

# --- IntLiteral -------------------------------------------------------------
is RakuAST::IntLiteral.new(42).gist, 'RakuAST::IntLiteral.new(42)',
    'IntLiteral.new gist round-trips';
is RakuAST::IntLiteral.new(42).value, 42, 'constructed IntLiteral.value is 42';
ok RakuAST::IntLiteral.new(42) ~~ RakuAST::Expression, 'a constructed node isa Expression';

# --- StrLiteral -------------------------------------------------------------
is RakuAST::StrLiteral.new("hi").gist, 'RakuAST::StrLiteral.new("hi")',
    'StrLiteral.new gist round-trips';
is RakuAST::StrLiteral.new("hi").value, 'hi', 'constructed StrLiteral.value is "hi"';

# --- RatLiteral -------------------------------------------------------------
is RakuAST::RatLiteral.new(3.5).gist, 'RakuAST::RatLiteral.new(3.5)',
    'RatLiteral.new gist round-trips';
