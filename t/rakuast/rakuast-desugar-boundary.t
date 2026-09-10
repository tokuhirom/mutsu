use v6;
use experimental :rakuast;
use Test;

# mutsu desugars a few constructs in the parser into calls to internal routines
# (`__mutsu_hyper_prefix`, `__mutsu_zip_assign`, …) or into temporaries with
# internal names (`__with_tmp_0`, `@__destructure_tmp__`). raku keeps every one
# of them as a dedicated RakuAST node and has no such name anywhere, so
# rendering one emitted a node that cannot exist in a real RakuAST tree — a
# `RakuAST::Call::Name.new(name => RakuAST::Name.from-identifier(
# "__mutsu_hyper_prefix"))` for `-<<@a`, for instance.
#
# Those are now explicit `.AST` coverage boundaries, which is the rule the rest
# of the converter follows: an erased distinction is a boundary, never a guess.
# The underlying constructs are tracked as read-direction gaps in
# todo/deep/rakuast-remaining.md.
#
# This file is mutsu-only: it asserts on mutsu's boundary behaviour, which has
# no raku counterpart (raku renders these constructs properly). It deliberately
# does NOT assert what the right node would be — that needs measuring against
# raku first.

plan 6;

# --- a desugared construct throws rather than rendering a fake node ---------
throws-like { Q[-<<@a].AST }, Exception,
    'hyper prefix does not render an internal __mutsu_hyper_prefix call';

throws-like { Q[my @a; @a Z= @a].AST }, Exception,
    'zip assignment does not render an internal __mutsu_zip_assign call';

throws-like { Q[with 1 { say $_ }].AST }, Exception,
    '`with` does not render its __with_tmp_N temporary';

# --- constructs that are NOT desugared still render -------------------------
ok Q[say 42].AST.gist.contains('RakuAST::Call::Name::WithoutParentheses.new('),
    'an ordinary named call still renders';

ok Q[my @a; @a>>.abs].AST.gist.contains('RakuAST::MetaPostfix::Hyper.new('),
    'a hyper method call still renders';

ok Q[my $x = 1].AST.gist.contains('RakuAST::VarDeclaration::Simple.new('),
    'an ordinary variable declaration still renders';
