use Test;

# Whitelisted roast files that behaved differently under the RakuAST frontend
# (MUTSU_RAKUAST=1) than on the ordinary one, each reduced to its cause. The
# expectations are rakudo 2026.09's.

plan 21;

sub flat($node) { $node.raku.lines.map(*.trim).join(' ').subst('RakuAST::', '', :g) }

# `not(...)` is a call of the sub; the chain inside it stays a chain.
isa-ok 'not(1 < 0 < 1)'.AST.statements[0].expression, RakuAST::Call::Name, 'not(...) is a Call::Name';
is EVAL('not(1 < 0 < 1)'.AST), True, 'the chain inside not(...) is still a chain';
is EVAL('my $x = 0; my $y = 0; not(++$x < $y++ < $y++)'.AST), True, 'a short-circuiting chain under not(...)';
isa-ok '!(1 < 0 < 1)'.AST.statements[0].expression, RakuAST::ApplyPrefix, '!(...) stays an ApplyPrefix';

# A chain that ends in a regex smartmatch is still one chain.
is EVAL('0 == 0 ~~ /0/'.AST).Str, '0', 'chained == and ~~ with a regex';
is EVAL('0 ~~ 0 ~~ /0/'.AST).Str, '0', 'chained ~~ and ~~ with a regex';

# A negated infix keeps the operands themselves and primes a Whatever.
is EVAL('my $foo; my $a = [$foo]; $a[0] !=:= $foo'.AST), True, '!=:= compares containers';
is EVAL('(* !< 3)'.AST).^name, 'WhateverCode', '* !< 3 is a WhateverCode';
is EVAL('(* !< 3)(2)'.AST), False, 'which negates the comparison';

# A term declaration's initializer reads terms, it declares none.
is EVAL('sub f { my \term:<ab> = e; ab }; f'.AST), e, 'my \term:<ab> = e keeps the constant e';

# The True keyword redeclared in a block is that block's.
is EVAL('{ constant True = 42; True }'.AST), 42, 'constant True = 42 inside a block';
is EVAL('{ constant True = 42 }; True'.AST), True, 'but not outside the block';

# `Any` is the type object, so a type-constrained call sees a coercion.
isa-ok EVAL('class A { method CALL-ME(A:U:) { 3 } }; A(Any).HOW'.AST), Metamodel::CoercionHOW, 'A(Any) is a type coercion literal';

# A loop label as a `Seq.from-loop` argument is a Label value.
lives-ok { EVAL('@ = (L3: Seq.from-loop({ loop { last L3 } }, { True }, :label(L3)))'.AST) }, ':label(L3) with a Label value';
ok flat('L: while 1 { last L }'.AST.statements[0]).contains('Label.new("L")'), 'a Label holds its name positionally';

# A single-quoted regex atom stays single-quoted.
is EVAL(q[my $r = /\w+ '\n&sin(3)'/; 'x\n&sin(3)' ~~ $r].AST).Str, 'x\n&sin(3)', 'a single-quoted atom does not interpolate &sin(3)';

# `<&Pkg::name>` is a package-qualified routine.
is EVAL('grammar D { our token doo { doo }; }; "doo" ~~ / <&D::doo> /'.AST).Str, 'doo', '<&D::doo> calls the qualified token';
my $src2 = 'grammar D2 { our token doo { doo }; }; "doo" ~~ / <&D2::doo> /';
ok flat($src2.AST.statements[1].expression).contains('Var::Package'), 'its callee is a Var::Package';

# Class traits keep the written order of is / does / hides.
my $decl = 'role R3a { }; role R3b { }; ';
ok flat(($decl ~ 'my class C3 hides R3a is R3b { }').AST.statements[2].expression).contains('Trait::Hides.new( Type::Simple.new( Name.from-identifier("R3a") ) ), Trait::Is.new'), 'hides before is';
ok flat(($decl ~ 'my class C3 is R3b hides R3a { }').AST.statements[2].expression).contains('Trait::Is.new( type => Type::Simple.new( Name.from-identifier("R3b") ) ), Trait::Hides'), 'is before hides';

# A placeholder in a routine with its own signature is an error while the tree is built.
throws-like { 'sub () { $^foo }.(42)'.AST }, X::Signature::Placeholder, 'a placeholder under an explicit empty signature';
