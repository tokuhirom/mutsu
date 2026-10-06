use Test;

# Parameters, declarator lists, anonymous routines and a few declarations in
# RakuAST, measured on rakudo 2026.09:
#
# - `+@a` / `+$a` / `+%a` carry `slurpy => Slurpy::SingleArgument`; a `where`
#   follows the slurpy marker;
# - a literal parameter (`sub f(1)`, `-> "a" { }`) has no target, the literal's
#   type (`Int`, `Str`, ...) and the literal as its `value`;
# - a parameter's trait is `Trait::Is(name [, argument])`, and `$x? = 3` has
#   `optional => True` beside its `default`;
# - the elements of `my (Int $a, \b, *@r)` are typed, sigilless (a
#   `ParameterTarget::Term`) or slurpy parameters; a `:=` list has no
#   `default-rw`;
# - an anonymous `sub () is rw { }` / `method () is rw { }` has the trait; a
#   method literal's declared invocant (`method (Foo:D $x: $y)`) is a leading
#   `Parameter(invocant => True)`;
# - `our Mu constant X = 1` has a `type`, `my Int $n where * > 0` a `where`;
#   a role has its own traits (`role R is tagged(5)`).
#
# The round trip is the parsed program. The tree part of this file also passes
# under `raku`; the round trip part is mutsu's.

plan 66;

sub exprs($src) { $src.AST.statements.map(*.expression) }
sub params($src) { exprs($src)[0].signature.parameters }
sub same($src, $expected, $desc) {
    my $parsed = do { my @*LOG; EVAL($src) };
    my $round = do { my @*LOG; EVAL($src.AST) };
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- single-argument slurpies
{
    my @p = params(Q[sub f1(+@a) { }]), params(Q[sub f2(+$a) { }]), params(Q[sub f3(+%a) { }]);
    is @p[0][0].slurpy.^name, 'RakuAST::Parameter::Slurpy::SingleArgument', '`+@a` is a single-argument slurpy';
    nok @p[0][0].type.defined, 'with no implicit type';
    is @p[1][0].slurpy.^name, 'RakuAST::Parameter::Slurpy::SingleArgument', '`+$a` too';
    isa-ok @p[1][0].type, RakuAST::Type::Setting, 'and the implicit `Any`';
    is @p[2][0].slurpy.^name, 'RakuAST::Parameter::Slurpy::SingleArgument', '`+%a` too';
    my $w = params(Q[sub f4(*@a where *.elems > 1) { }])[0];
    isa-ok $w.where, RakuAST::ApplyInfix, 'a slurpy `where` is a field of the parameter';
}

# --- literal values
{
    my @l = params(Q[sub l1(1, "a", 2.5) { }]);
    isa-ok @l[0].type, RakuAST::Type::Simple, 'a literal parameter has a type';
    is @l[0].type.name.canonicalize, 'Int', 'the literal\'s own';
    is @l[0].value, 1, 'and its value';
    nok @l[0].target.defined, 'and no target';
    is @l[1].type.name.canonicalize, 'Str', 'a string literal';
    is @l[2].type.name.canonicalize, 'Rat', 'a rational literal';
    my $p = exprs(Q[my $f = -> "about" { 1 }])[0].initializer.expression.signature.parameters[0];
    is $p.type.name.canonicalize, 'Str', 'a pointy block\'s literal parameter has the type too';
    is $p.value, 'about', 'and the value';
}

# --- traits and defaults
{
    my @t = exprs(Q[multi sub trait_mod:<is>(Parameter $p, :$marked!) { }; sub t1($x is marked(3), $y is marked, $z is copy) { }])[1].signature.parameters;
    is @t[0].traits[0].name.canonicalize, 'marked', 'a custom parameter trait';
    isa-ok @t[0].traits[0].argument, RakuAST::Circumfix::Parentheses, 'with its argument';
    nok @t[1].traits[0].argument.defined, 'a bare one has none';
    is @t[2].traits[0].name.canonicalize, 'copy', 'beside a builtin';
    my $o = params(Q[sub o1(Int $x? = 3) { }])[0];
    ok $o.optional, '`$x? = 3` is optional';
    isa-ok $o.default, RakuAST::IntLiteral, 'and has its default';
}

# --- declarator lists
{
    my @a = exprs(Q[my (Int $a, $b) = 1, 2; my ($c, *@r) = 1, 2, 3; my (\d, \e) := (1, 2); my ($f is rw, $g) = 1, 2; my Int ($h, $i) = 1, 2]);
    my @p = @a.map(*.signature.parameters);
    isa-ok @p[0][0].type, RakuAST::Type::Simple, 'a typed element';
    nok @p[0][1].type.defined, 'beside an untyped one';
    is @p[1][1].slurpy.^name, 'RakuAST::Parameter::Slurpy::Flattened', 'a slurpy element';
    isa-ok @p[2][0].target, RakuAST::ParameterTarget::Term, 'a sigilless element is a Term target';
    nok @p[2][0].default-rw, 'and a `:=` list has no default-rw';
    ok @p[0][0].default-rw, 'where `=` has';
    is @p[3][0].traits[0].name.canonicalize, 'rw', 'an element\'s `is rw`';
    isa-ok @a[4].signature.returns, RakuAST::Type::Simple, 'the declaration\'s own type is the signature\'s `returns`';
    isa-ok @a[4].type, RakuAST::Type::Simple, 'and the declaration\'s `type`';
}

# --- anonymous routines and invocants
{
    my @r = exprs(Q[my $f1 = sub () is rw { 1 }; my $f2 = method () is rw { 1 }; my $f3 = sub ($x) is rw { 1 }]).map(*.initializer.expression);
    is @r[0].traits[0].name.canonicalize, 'rw', '`sub () is rw`';
    isa-ok @r[1], RakuAST::Method, '`method () is rw` is a Method';
    is @r[1].traits[0].name.canonicalize, 'rw', 'with the trait';
    is @r[2].traits[0].name.canonicalize, 'rw', 'a parameterised one too';
    my @m = exprs(Q[my $m1 = method ($x: $y) { 1 }; my $m2 = method (Int:D $: $y) { 1 }; my $m3 = method ($y) { 1 }]).map(*.initializer.expression.signature.parameters);
    ok @m[0][0].invocant, 'a declared invocant is a parameter';
    is @m[0].elems, 2, 'ahead of the others';
    isa-ok @m[1][0].type, RakuAST::Type::Definedness, 'its type kept';
    nok @m[2][0].invocant, 'a method literal with no declared invocant has none';
}

# --- constants, where, roles
{
    my @c = exprs(Q[our Mu constant TC1 = 5; my Int constant TC2 = 7]);
    isa-ok @c[0].type, RakuAST::Type::Simple, 'a typed constant has a type';
    is @c[1].scope, 'my', 'and its scope';
    my @w = exprs(Q[my Int $n where * > 0 = 3; my $m where 3]);
    isa-ok @w[0].where, RakuAST::ApplyInfix, '`my T $x where ...` has a where';
    isa-ok @w[0].initializer, RakuAST::Initializer::Assign, 'beside its initializer';
    isa-ok @w[1].where, RakuAST::IntLiteral, 'an untyped one too';
    my $r = exprs(Q[role RT1[::T] is array_type(T) { }])[0];
    is $r.traits[0].name.canonicalize, 'array_type', 'a role\'s own trait';
    isa-ok $r.traits[0].argument, RakuAST::Circumfix::Parentheses, 'with its argument';
}

# --- the round trip is the parsed program
same Q[sub f1(+@a) { @a.elems }; f1(1, 2, 3) ~ "," ~ f1([1, 2])], '3,2', 'a single-argument slurpy';
same Q[multi sub f3(1) { "one" }; multi sub f3(Int $x) { "other" }; f3(1) ~ f3(2)], 'oneother', 'a literal parameter dispatches';
same Q[multi sub f4("a") { "A" }; multi sub f4(Str $x) { "S" }; f4("a") ~ f4("b")], 'AS', 'a string literal parameter';
same Q[my $f = -> 3 { "three" }; $f(3)], 'three', 'a pointy block\'s literal parameter';
same Q[my $f = -> 3 { "three" }; (try { $f(4); "ok" }) // "fail"], 'fail', 'which rejects another value';
same Q[multi sub trait_mod:<is>(Parameter $p, :$marked!) { @*LOG.push("m:" ~ $marked.raku) }; sub f5($x is marked(3), $y is marked) { 1 }; @*LOG.join(",")], 'm:3,m:Bool::True', 'custom parameter traits run';
same Q[sub f6(Int $x? = 3) { $x }; f6()], 3, 'an optional parameter with a default';
same Q[sub f7(*@a where *.elems > 1) { @a.elems }; f7(1, 2)], 2, 'a slurpy `where` accepts';
same Q[sub f8(*@a where *.elems > 1) { @a.elems }; (try { f8(1); "ok" }) // "fail"], 'fail', 'and rejects';
same Q[my (Int $a, $b) = 1, 2; "$a$b"], '12', 'a typed declarator element';
same Q[(try { my (Int $a, $b) = "x", 2; "ok" }) // "fail"], 'fail', 'enforces its type';
same Q[my ($a, *@r) = 1, 2, 3; "$a|{@r}"], '1|2 3', 'a slurpy declarator element';
same Q[my (\a, \b) := (1, 2); a + b], 3, 'a sigilless declarator list';
same Q[my $x = 1; my ($a is rw, $b) := ($x, 2); $a = 5; $x], 5, 'an `is rw` element';
same Q[my $v = 1; my $f = sub () is rw { $v }; $f() = 5; $v], 5, 'an anonymous `is rw` sub';
same Q[my $v = 1; my $f = sub ($y) is rw { $v }; $f(0) = 7; $v], 7, 'with a parameter';
same Q[my $m = method ($x: $y) { "$x/$y" }; $m(3, 4)], '3/4', 'a declared invocant';
same Q[my $m = method (Int:D $: $y) { $y }; $m(3, 4)], 4, 'a typed anonymous invocant';
same Q[my Int constant TC2 = 7; TC2], 7, 'a typed constant';
same Q[my Int $n where * > 0 = 3; $n], 3, 'a `where` on a variable declaration';
same Q[(try { my Int $m where * > 0 = -1; "ok" }) // "fail"], 'fail', 'which is enforced';
same Q[multi sub trait_mod:<is>(Mu:U $r, :$tagged!) { @*LOG.push("t:" ~ $tagged.raku) }; role RT2 is tagged(5) { }; @*LOG.join(",")], 't:5', 'a role\'s own trait runs';
