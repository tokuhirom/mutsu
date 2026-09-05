use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# A bareword naming something the same compilation unit declared (ADR-0011).
#
# raku resolves such a name at parse time, so `class C { }; C.new` renders `C`
# as a `Type::Simple` — exactly like a builtin type — and `constant X = 5; X`
# renders `X` as a `Term::Name`. Both measured against rakudo 2026.07.
#
# mutsu's parser leaves both as `Expr::BareWord`, and only *builtin* type names
# converted, so any program that declared a class or a constant and then used it
# was a `.AST` boundary. That is what forced every declaration test so far to
# inspect the EVAL'd value from the outside instead of using the name inside the
# lowered program — the tests below do use it.
#
# Also fixed here: `.^name` is a *metamethod* call (`Call::MetaMethod`, whose
# `name` is a plain string), not an ordinary call carrying a `.^` dispatch
# modifier. Only `.?` / `.+` / `.*` are dispatch modifiers.
#
# Each test uses distinct names because `.AST` registers the symbol and raku
# rejects redeclaration.
#
# Passes under BOTH mutsu and raku.

plan 14;

# --- read: a declared type renders like a builtin one ------------------------
my $cls = Q{class D1 { }; D1.new}.AST.gist;
ok $cls.contains('RakuAST::Type::Simple.new(') && $cls.contains('RakuAST::Name.from-identifier("D1")'),
    'a declared class used as a term renders as Type::Simple';

ok Q{role D2 { }; D2.^name}.AST.gist.contains('RakuAST::Type::Simple.new('),
    'a declared role used as a term renders as Type::Simple';

# --- read: a declared constant renders as Term::Name -------------------------
my $const = Q{constant D3 = 5; D3}.AST.gist;
ok $const.contains('RakuAST::Term::Name.new('),
    'a declared constant used as a term renders as Term::Name';
nok $const.contains('RakuAST::Type::Simple.new(
    RakuAST::Name.from-identifier("D3")'),
    'a declared constant is not rendered as a type';

# --- read: `.^name` is a metamethod call -------------------------------------
my $meta = Q{my $x = 1; $x.^name}.AST.gist;
ok $meta.contains('RakuAST::Call::MetaMethod.new('), '`.^name` renders a Call::MetaMethod';
ok $meta.contains('name => "name"'), 'a metamethod name is a plain string';
nok $meta.contains('dispatch'), '`.^` is not a dispatch modifier';

# --- read: `.?` is still an ordinary call with a dispatch modifier ------------
my $maybe = Q{my $x = 1; $x.?abs}.AST.gist;
ok $maybe.contains('RakuAST::Call::Method.new(') && $maybe.contains('dispatch => ".?"'),
    '`.?` is still a dispatch modifier on an ordinary Call::Method';

# --- write: the declared name is usable inside the lowered program -----------
is EVAL(Q{constant D4 = 5; D4}.AST), 5, 'a constant is readable by name';
is EVAL(Q{constant D5 = 5; D5 + 1}.AST), 6, 'a constant works in an expression';
is EVAL(Q{my constant D6 = "ab"; D6.chars}.AST), 2, 'a lexical constant is readable by name';
is EVAL(Q{class D7 { method m() { 7 } }; D7.m}.AST), 7, 'a declared class is callable by name';
is EVAL(Q{class D8 { has $.v; method d() { $!v * 2 } }; D8.new(v => 21).d}.AST), 42,
    'a declared class can be constructed and used by name';
is EVAL(Q{role D9R { method m() { 5 } }; class D9 does D9R { }; D9.new.m}.AST), 5,
    'a class composing a role is usable by name';
