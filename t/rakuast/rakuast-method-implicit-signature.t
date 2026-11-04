use Test;

# #16423: `visit-children` of a `Method` shows the Signature rakudo fills in:
# a leading invocant parameter (typed by the enclosing class or role, bare
# outside a class) unless the written signature opens with an invocant of its
# own, and a trailing `%_` unless the written signature collects named
# arguments (`*%h`, `|c`). A `Var::Attribute::Public` (`$.n`) visits its
# `self.n.item` expansion (`.list` for `@`, `.hash` for `%`). Shapes measured
# on rakudo 2026.09; none of the synthesized nodes has an origin.

plan 17;

sub shape($node) {
    my @kids;
    $node.visit-children: -> $c { @kids.push: shape($c) };
    $node.^name.subst('RakuAST::', '') ~ (@kids ?? '(' ~ @kids.join(',') ~ ')' !! '')
}

# The first node of class `$class` in `$src`.
sub first-of(Str $src, Str $class) {
    my $found;
    sub walk($n) {
        $found //= $n if $n.^name eq $class;
        $n.visit-children: &walk unless $found;
    }
    $src.AST.visit-children(&walk);
    $found
}

# The shape of the Signature a method (or submethod) visits.
sub signature-shape(Str $src) {
    my $method = first-of($src, $src.contains('submethod') ?? 'RakuAST::Submethod' !! 'RakuAST::Method');
    my $shape;
    $method.visit-children: -> $c { $shape //= shape($c) if $c.^name eq 'RakuAST::Signature' };
    $shape
}

my $inv = 'Parameter(Type::Simple(Name))';
my $rest = 'Parameter(ParameterTarget::Var(VarDeclaration::Simple))';

is signature-shape('class C1 { method m { 1 } }'), "Signature($inv,$rest)",
    'a method without a signature gets the invocant and %_';
is signature-shape('class C2 { method m($x) { 1 } }'),
    "Signature($inv,Parameter(Type::Setting(Name),ParameterTarget::Var(VarDeclaration::Simple)),$rest)",
    'a written parameter sits between them';
is signature-shape('class C3 { submethod s { 1 } }'), "Signature($inv,$rest)",
    'a submethod too';
is signature-shape('role R1 { method m { 1 } }'), "Signature($inv,$rest)",
    'a role method';
is signature-shape('class C4 { method m(*%h) { 1 } }'),
    "Signature($inv,$rest)",
    'a slurpy hash replaces %_';
is signature-shape('class C5 { method m(*@a) { 1 } }'),
    "Signature($inv,$rest,$rest)",
    'a slurpy array does not';
is signature-shape('class C6 { method m(|c) { 1 } }'),
    "Signature($inv,Parameter(Type::Setting(Name),ParameterTarget::Term(Name)))",
    'a capture replaces %_';
is signature-shape('class C7 { method m(Int:D: $x) { 1 } }'),
    "Signature(Parameter(Type::Definedness(Type::Simple(Name))),Parameter(Type::Setting(Name),ParameterTarget::Var(VarDeclaration::Simple)),$rest)",
    'a written invocant is not preceded by another';
is signature-shape('class C8 { method m($self: $x) { 1 } }'),
    "Signature(Parameter(Type::Setting(Name),ParameterTarget::Var(VarDeclaration::Simple)),Parameter(Type::Setting(Name),ParameterTarget::Var(VarDeclaration::Simple)),$rest)",
    'a named written invocant too';
is signature-shape('grammar G1 { method m { 1 } }'),
    "Signature(Parameter,$rest)",
    'a grammar method has a bare invocant parameter';

my $implicit;
first-of('class C9 { method m { 1 } }', 'RakuAST::Method').visit-children: -> $c {
    $implicit = $c if $c.^name eq 'RakuAST::Signature';
};
ok !$implicit.origin.defined, 'the implicit signature has no origin';

my $m = first-of('class C10 { method m($x) { 1 } }', 'RakuAST::Method');
is $m.signature.parameters.elems, 1, 'the signature accessor still shows only the written parameter';

# `$.n` and friends.
my $expansion = 'ApplyPostfix(ApplyPostfix(Term::Self,Call::Method(Name,ArgList)),Call::Method(Name,ArgList))';
is shape(first-of('class C11 { method m { $.n } }', 'RakuAST::Var::Attribute::Public')),
    "Var::Attribute::Public($expansion)", '$.n visits its expansion';
my @names;
first-of('class C12 { method m { %.h } }', 'RakuAST::Var::Attribute::Public').visit-children: -> $c {
    $c.operand.postfix.visit-children: -> $n { @names.push: $n.raku if $n.^name eq 'RakuAST::Name' };
    @names.push: $c.postfix.name.raku;
}
is @names.elems, 2, 'the expansion holds the accessor and the coercion';
like @names[1], /'"hash"'/, '%.h coerces with .hash';

# The constructor form and the EVAL round trip are unchanged.
my $ast = 'class C13 { has $.n; method m { $.n } }'.AST;
lives-ok { EVAL($ast) }, 'EVAL of the tree still defines the class';
is $ast.raku.contains('Signature'), False, 'the constructor form shows no signature';
