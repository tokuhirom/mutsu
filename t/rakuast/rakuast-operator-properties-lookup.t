use Test;
use experimental :rakuast;

# The class-level `OperatorProperties.infix / prefix / postfix / circumfix /
# postcircumfix` lookups (#14916, ADR-14916), measured on rakudo 2026.09.

plan 31;

my $c = RakuAST::Infix.new("+").properties.WHAT;
sub strip(OperatorProperties:D $p) { $p.raku.subst('OperatorProperties.new: ', '') }

is $c.^name, 'OperatorProperties', 'the class behind .properties';

# --- infix --------------------------------------------------------------
is strip($c.infix("+")), ':precedence("t="), :associative("left"), :dba("additive")', 'infix +';
is strip($c.infix("==")),
    ':precedence("m="), :associative("chain"), :dba("chaining"), :iffy, :diffy', 'infix ==';
is strip($c.infix("ff")),
    ':precedence("j="), :associative("right"), :thunky("tt"), :dba("conditional-ff"), :iffy',
    'infix ff is a flip-flop';
is strip($c.infix(".")),
    ':precedence("v="), :sub-precedence("z="), :associative("left"), :dba("dotty-infix"), :next-term("dottyopish"), :fiddly',
    'infix . is the dotty infix';
is strip($c.infix("nope")), ':precedence("t="), :associative("left"), :dba("default-infix")',
    'an unknown infix gets the default';
is strip($c.infix), strip($c.infix("")), 'the symbol defaults to the empty string';

# --- prefix / postfix ---------------------------------------------------
is strip($c.prefix("-")), ':precedence("v="), :associative("unary"), :dba("symbolic-unary")', 'prefix -';
is strip($c.prefix("++")), ':precedence("x="), :associative("unary"), :dba("autoincrement")', 'prefix ++';
is strip($c.prefix("nope")), ':precedence("v="), :associative("unary"), :dba("default-prefix")',
    'an unknown prefix gets the default';
is strip($c.postfix("++")), ':precedence("x="), :associative("unary"), :dba("autoincrement")', 'postfix ++';
is strip($c.postfix(".")),
    ':precedence("y="), :associative("unary"), :dba("methodcall"), :fiddly', 'postfix .';
is strip($c.postfix("nope")), ':precedence("x="), :associative("unary"), :dba("default-postfix")',
    'an unknown postfix gets the default';

# --- circumfix / postcircumfix -------------------------------------------
my $methodcall = ':precedence("y="), :associative("unary"), :dba("methodcall"), :fiddly';
for '[ ]', '< >', '<< >>', '« »', '{ }' -> $s {
    is strip($c.circumfix($s)), $methodcall, "circumfix $s is the method-call level";
}
is strip($c.circumfix("( )")), ':dba("default-circumfix")', 'circumfix ( ) gets the default';
is strip($c.circumfix("[; ]")), ':dba("default-circumfix")', 'circumfix [; ] is not known';
is strip($c.postcircumfix("[ ]")), $methodcall, 'postcircumfix [ ]';
is strip($c.postcircumfix("[; ]")), $methodcall, 'postcircumfix [; ]';
is strip($c.postcircumfix("{; }")), $methodcall, 'postcircumfix {; }';
is strip($c.postcircumfix("< >")), $methodcall, 'postcircumfix < >';
is strip($c.postcircumfix("( )")), ':precedence("y="), :dba("default-postcircumfix")',
    'postcircumfix ( ) gets the default';

# --- agreement with the operator nodes ------------------------------------
is $c.infix("**").raku, RakuAST::Infix.new("**").properties.raku, 'infix agrees with the node';
is $c.prefix("so").raku, RakuAST::Prefix.new("so").properties.raku, 'prefix agrees with the node';
is $c.postfix("--").raku, RakuAST::Postfix.new(:operator("--")).properties.raku,
    'postfix agrees with the node';

# --- the answers are real objects -----------------------------------------
isa-ok $c.infix("+"), $c, 'an OperatorProperties instance';
is $c.infix("+").precedence, 't=', 'accessor precedence';
is $c.infix("+").dba, 'additive', 'accessor dba';
