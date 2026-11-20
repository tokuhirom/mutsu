use Test;
use experimental :rakuast;

plan 26;

role A { }
role B { }
multi trait_mod:<is>(Mu:U $s, :$tag!) { }

# An enum's and a subset's traits keep their written order (#14845; measured
# on rakudo 2026.11).
is Q[enum O1 is export does A <o1 o2>].AST.DEPARSE.trim, 'enum O1 is export does A <o1 o2>', 'is export before does';
is Q[enum O2 does A is export does B <o1 o2>].AST.DEPARSE.trim, 'enum O2 does A is export does B <o1 o2>', 'is export between roles';
is Q[enum O3 does A is export <o1>].AST.DEPARSE.trim, 'enum O3 does A is export <o1>', 'is export after does';
is Q[subset S1 of Int is export where * > 0].AST.DEPARSE.trim, 'subset S1 of Int is export where * > 0', 'of before is export';
is Q[subset S2 is export of Int where * > 0].AST.DEPARSE.trim, 'subset S2 is export of Int where * > 0', 'is export before of';
is Q[subset S3 of Int is tag where * > 0].AST.statements[0].expression.traits.map(*.^name).join(','),
    'RakuAST::Trait::Of,RakuAST::Trait::Is', 'of before a custom trait';

# A `< >` enum body keeps its spacing.
is Q[enum Colors < Red Green Blue >].AST.DEPARSE.trim, 'enum Colors < Red Green Blue >', 'padded words';
is Q[enum C2 <Red  Green>].AST.DEPARSE.trim, 'enum C2 <Red  Green>', 'two spaces';
is Q[enum C3 <Red Green>].AST.DEPARSE.trim, 'enum C3 <Red Green>', 'single spaces';
is Q[enum C4 < >].AST.DEPARSE.trim, 'enum C4 < >', 'an empty body';
is EVAL(Q[enum C5 < Red Green >; C5::Green.value].AST), 1, 'EVAL of a padded body';

# `constant X := EXPR` keeps its `Initializer::Bind`.
is Q[my constant %bound := hash].AST.DEPARSE.trim, 'my constant %bound := hash', 'constant :=';
is Q[my constant Z := 42].AST.statements[0].expression.initializer.^name, 'RakuAST::Initializer::Bind', 'Initializer::Bind';
is Q[constant Y = 42].AST.DEPARSE.trim, 'constant Y = 42', 'constant = stays';
is EVAL(Q[my constant W := 42; W].AST), 42, 'EVAL of constant :=';
my constant %h := hash a => 1;
is %h<a>, 1, 'constant := still binds';

# A hyper superscript power and the hyper slip keep their meta prefix/postfix.
my @a = 1, 2;
is Q[@a>>²].AST.DEPARSE.trim, '@a>>²', '@a>>²';
is Q[@a>>²].AST.statements[0].expression.postfix.^name, 'RakuAST::MetaPostfix::Hyper', 'MetaPostfix::Hyper';
is-deeply EVAL(Q[my @b = 1, 2, 3; @b>>²].AST), [1, 4, 9], 'EVAL of @b>>²';
is Q[join |<< <a x y z>].AST.DEPARSE.trim, 'join |<<<a x y z>', 'the hyper slip renders as rakudo does';
is Q[join |<< <a x y z>].AST.statements[0].expression.args.args[0].prefix.^name, 'RakuAST::MetaPrefix::Hyper', 'MetaPrefix::Hyper';
is (join "-", |<< <a x y z>), 'a-x-y-z', 'the hyper slip still slips';

# `&.name` is the public attribute access, with or without arguments.
is Q[class K1 { has &.cb; method m { &.cb.(1) } }].AST.DEPARSE.lines[3].trim, '&.cb.(1)', '&.cb.(1)';
is Q[class K2 { has &.cb; method m { &.cb(2) } }].AST.DEPARSE.lines[3].trim, '&.cb(2)', '&.cb(2)';
class K3 { has &.cb; method m { &.cb.(5) } }
is K3.new(cb => * + 1).m, 6, '&.cb still calls the accessor';
is EVAL(Q[class K4 { has &.cb; method m { &.cb()(7) } }; K4.new(cb => * * 2).m].AST), 14, 'EVAL of &.cb()';
