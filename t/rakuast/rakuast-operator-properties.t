use Test;
use experimental :rakuast;

# `properties`, `reducer-name` and `default-operator-properties` of the
# RakuAST operator nodes (#14916, ADR-14916), measured on rakudo 2026.09.

plan 49;

sub infix($s) { RakuAST::Infix.new($s) }
sub props(RakuAST::Node $n) { $n.properties.raku }

# --- Infix: the built-in precedence levels -------------------------------
is props(infix("+")), 'OperatorProperties.new: :precedence("t="), :associative("left"), :dba("additive")',
    '+ is additive';
is props(infix("**")), 'OperatorProperties.new: :precedence("w="), :associative("right"), :dba("exponentiation")',
    '** is right-associative exponentiation';
is props(infix("&&")),
    'OperatorProperties.new: :precedence("l="), :associative("left"), :thunky(".t"), :dba("tight-and"), :iffy',
    '&& is thunky and iffy';
is props(infix("~~")),
    'OperatorProperties.new: :precedence("m="), :associative("chain"), :dba("chaining"), :iffy, :diffy',
    '~~ is a chaining operator';
is props(infix(",")),
    'OperatorProperties.new: :precedence("g="), :associative("list"), :dba("comma"), :next-term("nulltermish")',
    ', has a next-term';
is props(infix("=")),
    'OperatorProperties.new: :precedence("i="), :sub-precedence("e="), :associative("right"), :dba("list assignment"), :fiddly',
    '= is list assignment';
is props(infix(":=")),
    'OperatorProperties.new: :precedence("i="), :sub-precedence("e="), :associative("right"), :dba("list assignment"), :fiddly',
    ':= is parsed by the grammar and answers the list assignment level';
is props(infix("ff")),
    'OperatorProperties.new: :precedence("j="), :associative("right"), :thunky("tt"), :dba("conditional-ff"), :iffy',
    'ff is a flip-flop';
is props(infix("zzz")),
    'OperatorProperties.new: :precedence("t="), :associative("left"), :dba("default-infix")',
    'an unknown infix gets the default';
is props(infix("\x2264")),
    'OperatorProperties.new: :precedence("t="), :associative("left"), :dba("default-infix")',
    'the Unicode <= is not in rakudo\'s operator table';

# --- accessors -----------------------------------------------------------
my $p = infix("&&").properties;
isa-ok $p, OperatorProperties, 'properties is an OperatorProperties';
is $p.precedence, 'l=', 'precedence';
is $p.associative, 'left', 'associative';
is $p.thunky, '.t', 'thunky';
is $p.dba, 'tight-and', 'dba';
is $p.iffy, 1, 'iffy';
is $p.diffy, 0, 'diffy';
is $p.sub-precedence, '', 'an absent string attribute is empty';
is infix("=").properties.fiddly, 1, 'fiddly';
is $p.gist, $p.raku, 'gist is raku';
is OperatorProperties.raku, 'OperatorProperties', 'the type object';
is OperatorProperties.new(:precedence<a=>, :iffy).raku,
    'OperatorProperties.new: :precedence("a="), :iffy', 'a hand-built one';

# --- reducer-name ---------------------------------------------------------
is infix("+").reducer-name, '&METAOP_REDUCE_LEFT', 'left folds left';
is infix("**").reducer-name, '&METAOP_REDUCE_RIGHT', 'right folds right';
is infix("==").reducer-name, '&METAOP_REDUCE_CHAIN', 'chain';
is infix(",").reducer-name, '&METAOP_REDUCE_LIST', 'list';
is infix("Z").reducer-name, '&METAOP_REDUCE_LISTINFIX', 'list-infix';

# --- prefix, postfix, postcircumfix, dotty -------------------------------
is RakuAST::Prefix.new("-").properties.dba, 'symbolic-unary', 'prefix -';
is RakuAST::Prefix.new("not").properties.precedence, 'h=', 'prefix not';
is RakuAST::Prefix.new("zzz").properties.dba, 'default-prefix', 'unknown prefix';
is RakuAST::Prefix.new("-").reducer-name, '&METAOP_REDUCE_LEFT', 'a prefix reduces left';
is RakuAST::Prefix.new("-").default-operator-properties.raku, props(RakuAST::Prefix.new("-")),
    'default-operator-properties is the looked-up level';
is RakuAST::Postfix.new(:operator<++>).properties.dba, 'autoincrement', 'postfix ++';
is RakuAST::Postfix.new(:operator<i>).properties.dba, 'methodcall', 'postfix i';
is RakuAST::Postcircumfix::ArrayIndex.new(:index(RakuAST::SemiList.new)).properties.raku,
    'OperatorProperties.new: :precedence("y="), :associative("unary"), :dba("methodcall"), :fiddly',
    'an array subscript is a method call';
is RakuAST::DottyInfix::Call.new.properties.raku,
    'OperatorProperties.new: :precedence("v="), :sub-precedence("z="), :associative("left"), :dba("dotty-infix"), :next-term("dottyopish"), :fiddly',
    'the dotty infix';

# --- metaoperators --------------------------------------------------------
is props(RakuAST::MetaInfix::Assign.new(infix("+"))),
    'OperatorProperties.new: :precedence("i="), :associative("right"), :dba("item-assignment")',
    '+= is item assignment';
is RakuAST::MetaInfix::Assign.new(infix("+")).reducer-name, '&METAOP_REDUCE_LEFT', '+= folds like +';
is RakuAST::MetaInfix::Assign.new(infix("**")).reducer-name, '&METAOP_REDUCE_RIGHT', '**= folds like **';
is RakuAST::MetaInfix::Assign.new(infix(",")).properties.dba, 'list assignment',
    ',= is list assignment (the inner infix binds looser than assignment)';
is RakuAST::MetaInfix::Assign.new(infix("or")).properties.dba, 'list assignment', 'or= too';
is RakuAST::MetaInfix::Assign.new(infix("||")).properties.dba, 'item-assignment', '||= is not';
is props(RakuAST::MetaInfix::Negate.new(infix("=="))), props(infix("==")), '!== answers ==';
is props(RakuAST::MetaInfix::Reverse.new(infix("+"))),
    'OperatorProperties.new: :precedence("t="), :associative("right"), :dba("additive")',
    'R+ flips the associativity';
is RakuAST::MetaInfix::Reverse.new(infix("**")).reducer-name, '&METAOP_REDUCE_LEFT', 'R** reduces left';
is props(RakuAST::MetaInfix::Cross.new(infix("+"))),
    'OperatorProperties.new: :precedence("f="), :associative("list"), :dba("list-infix")', 'X+ is list-infix';
is RakuAST::MetaInfix::Zip.new(infix("+")).reducer-name, '&METAOP_REDUCE_LISTINFIX', 'Z+';
is props(RakuAST::MetaInfix::Hyper.new(:infix(infix("+")))), props(infix("+")), '>>+<< answers +';
is props(RakuAST::FunctionInfix.new(RakuAST::Var::Lexical.new('&foo'))), props(infix("+")),
    'a function infix answers the additive level';
