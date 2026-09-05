use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# Three more read gaps, all measured against rakudo 2026.07 and byte-for-byte
# identical there:
#
#   * class traits — `is Parent` is `Trait::Is(type => Type::Simple)` (a NAMED
#     `type`), `does Role` is `Trait::Does(Type::Simple)` (POSITIONAL), and
#     `is rw` is `Trait::Is(name => Name)`, a trait *name* rather than a type.
#     `is repr(...)` is its own leaf field, not a trait.
#   * `multi sub` — `multiness => "multi"`, before `name` in field order.
#   * `constant X = 5` — `VarDeclaration::Constant`, whose `name` is a plain
#     string rather than a `Name` node. The package-scoped default spelling
#     emits no `scope`; `my constant` emits `scope => "my"`.
#
# Each declaration is the last statement of its EVAL'd program, so the EVAL'd
# value is the declared thing and the test inspects it from the outside.
# Referring to a user class or constant by bare name *inside* the same program
# is a separate, still-open read gap.
#
# Passes under BOTH mutsu and raku.

plan 16;

# --- class traits: read ------------------------------------------------------
my $is = Q{class T1 is Int { }}.AST.gist;
ok $is.contains('RakuAST::Trait::Is.new('), '`is Parent` renders a Trait::Is';
ok $is.contains('type => RakuAST::Type::Simple.new('),
    '`is Parent` carries its parent as a named `type`';

my $does = Q{role T2R { }; class T2 does T2R { }}.AST.gist;
ok $does.contains('RakuAST::Trait::Does.new('), '`does Role` renders a Trait::Does';
ok $does.contains("RakuAST::Name.from-identifier(\"T2R\")"),
    '`does Role` names the composed role';

my $rw = Q{class T3 is rw { }}.AST.gist;
ok $rw.contains('RakuAST::Trait::Is.new(') && $rw.contains('name => RakuAST::Name.from-identifier("rw")'),
    '`is rw` renders a Trait::Is carrying a trait *name*';

ok Q{class T4 is Int is repr("P6opaque") { }}.AST.gist.contains('repr   => "P6opaque"'),
    'a repr renders as its own leaf field';

nok Q{class T5 { }}.AST.gist.contains('traits'),
    'a plain class emits no traits field';

# --- multi: read -------------------------------------------------------------
ok Q{multi sub t6(Int $x) { 1 }}.AST.gist.contains('multiness => "multi"'),
    'a multi sub renders its multiness';
nok Q{sub t7($x) { 1 }}.AST.gist.contains('multiness'),
    'an ordinary sub emits no multiness';

# --- constant: read ----------------------------------------------------------
my $const = Q{constant T8 = 5}.AST.gist;
ok $const.contains('RakuAST::VarDeclaration::Constant.new('),
    'a constant renders as VarDeclaration::Constant';
ok $const.contains('name        => "T8"'), 'a constant names itself with a plain string';
nok $const.contains('scope'), 'a package-scoped constant emits no scope';
ok Q{my constant T9 = 7}.AST.gist.contains('scope       => "my"'),
    'a lexical constant emits scope => "my"';

# --- write side --------------------------------------------------------------
is EVAL(Q{constant TA = 5}.AST), 5, 'a constant lowers to its value';
is EVAL(Q{class TB is Int { }}.AST).^name, 'TB', 'a class with a parent lowers';
is EVAL(Q{role TCR { method m() { 6 } }; class TC does TCR { }}.AST).new.m, 6,
    'a class composing a role lowers, and the role method is callable';
