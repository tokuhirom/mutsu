use Test;

# Dynamic method calls and subscripts in RakuAST, measured on rakudo 2026.09:
#
# - `$o.$n(1)` is an `ApplyPostfix` whose postfix is `Call::TermAsMethod(callee =>
#   $n, args => ArgList(1))`; `.?` / `.*` is its `dispatch`;
# - `$o.&f(1)` is a `Call::NameAsMethod(name => f, args => ...)`;
# - `@a>>.$n()` wraps either in a `MetaPostfix::Hyper`;
# - `@a[0;1]` / `%h{1;2}` is the same postcircumfix as `@a[0]` with one
#   `SemiList` statement per dimension, and an assignment to it an `ApplyInfix`
#   over an `Assignment`;
# - `@a[0;1]:exists` is the same postcircumfix with a `ColonPair::True` among its
#   `colonpairs`, and a zen slice `@a[]` one with no dimension at all;
# - `$::($n)` / `@::($n)` is a `Var::Package` over a dynamic name, and `$::($n) = 5`
#   an `ApplyInfix` over `Assignment(:item)`.
#
# The round trip is the parsed program. The tree part of this file also passes
# under `raku`; the round trip part is mutsu's.

plan 53;

sub exprs($src) { ('my ($o, $n, $c); my @a; my %h; sub f(|) { }; ' ~ $src).AST.statements.skip(4).map(*.expression) }
sub same($src, $expected, $desc) {
    my $parsed = do { my @*LOG; EVAL($src) };
    my $round = do { my @*LOG; EVAL($src.AST) };
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- dynamic method calls
{
    my $d = exprs(Q[$o.$n()])[0];
    isa-ok $d, RakuAST::ApplyPostfix, 'a dynamic call is a postfix application';
    isa-ok $d.postfix, RakuAST::Call::TermAsMethod, 'of a term as a method';
    isa-ok $d.postfix.callee, RakuAST::Var::Lexical, 'whose callee is the variable';
    is exprs(Q[$o.$n(1, 2)])[0].postfix.args.args.elems, 2, 'with its arguments';
    is exprs(Q[$o.?$n()])[0].postfix.dispatch, '.?', 'and a `.?` dispatch';
    is exprs(Q[$o.*$n()])[0].postfix.dispatch, '.*', 'or a `.*`';
    my $f = exprs(Q[$o.&f(1)])[0];
    isa-ok $f.postfix, RakuAST::Call::NameAsMethod, '`.&f` is a name as a method';
    is $f.postfix.name.canonicalize, 'f', 'with the routine\'s name';
    is $f.postfix.args.args.elems, 1, 'and arguments';
    my $h = exprs(Q[@a>>.$n()])[0];
    isa-ok $h.postfix, RakuAST::MetaPostfix::Hyper, 'a hyper dynamic call';
    isa-ok $h.postfix.postfix, RakuAST::Call::TermAsMethod, 'over the same call node';
    isa-ok exprs(Q[@a>>.&f])[0].postfix.postfix, RakuAST::Call::NameAsMethod, 'and for `.&f`';
}

# --- multi-dimensional subscripts
{
    my $m = exprs(Q[@a[0;1]])[0];
    isa-ok $m.postfix, RakuAST::Postcircumfix::ArrayIndex, '`@a[0;1]` is an array index';
    is $m.postfix.index.statements.elems, 2, 'with a statement per dimension';
    isa-ok exprs(Q[%h{1;2}])[0].postfix, RakuAST::Postcircumfix::HashIndex, '`%h{1;2}` is a hash index';
    is exprs(Q[@a[0;1;2]])[0].postfix.index.statements.elems, 3, 'any number of dimensions';
    isa-ok exprs(Q[@a[*;1]])[0].postfix.index.statements[0].expression, RakuAST::Term::Whatever, 'a `*` dimension';
    my $s = exprs(Q[@a[0;1] = 5])[0];
    isa-ok $s, RakuAST::ApplyInfix, 'an assignment is an infix application';
    isa-ok $s.infix, RakuAST::Assignment, 'over an assignment';
    isa-ok $s.left.postfix, RakuAST::Postcircumfix::ArrayIndex, 'to the subscript';
}

# --- adverbs on a multi-dimensional subscript, zen slices
{
    my $e = exprs(Q[@a[0;1]:exists])[0];
    isa-ok $e.postfix, RakuAST::Postcircumfix::ArrayIndex, 'a multi-dimensional subscript with an adverb';
    isa-ok $e.postfix.colonpairs[0], RakuAST::ColonPair::True, 'has its colonpair';
    is $e.postfix.colonpairs[0].key, 'exists', 'named `exists`';
    is exprs(Q[@a[]])[0].postfix.index.statements.elems, 0, 'a zen slice has no dimension';
}

# --- symbolic dereference
{
    my $d = exprs(Q[$::($n)])[0];
    isa-ok $d, RakuAST::Var::Package, '`$::($n)` is a package variable';
    is $d.sigil, '$', 'with its sigil';
    isa-ok $d.name, RakuAST::Name, 'over a dynamic name';
    is exprs(Q[@::($n)])[0].sigil, '@', 'an array';
    my $s = exprs(Q[$::($n) = 5])[0];
    isa-ok $s, RakuAST::ApplyInfix, 'an assignment is an infix application';
    isa-ok $s.left, RakuAST::Var::Package, 'to the variable';
    isa-ok exprs(Q[::($n) = 5])[0].left, RakuAST::Term::Name, 'and `::($n) = 5` to a term name';
}

# --- the round trip is the parsed program
same Q[my $n = -> $s { $s.uc }; "abc".$n], 'ABC', 'a method call by a callable';
same Q[sub f($x, $y) { $x ~ $y }; "a".&f("b")], 'ab', 'a routine called as a method';
same Q[my $k = -> $s, $t { $s ~ $t }; "a".$k("b")], 'ab', 'a callable with an argument';
same Q[my $n = -> $s { $s.uc }; my @a = <a b>; (@a>>.$n).join(",")], 'A,B', 'a hyper dynamic call';
same Q[sub f($x, $y) { $x ~ $y }; my @a = <a b>; (@a>>.&f("x")).join(",")], 'ax,bx', 'a hyper `.&f`';
same Q[class Foo { method bar() { "bar" } }; my $o = Foo.new; my $m = "bar"; $o."$m"()], 'bar', 'an interpolated quoted name still works';
same Q[my @a = [[1, 2], [3, 4]]; @a[1;0]], 3, 'a multi-dimensional index';
same Q[my @b; @b[1;2] = 5; @b[1;2] ~ "," ~ @b.elems], '5,2', 'a multi-dimensional assignment';
same Q[my %h; %h{1;2} = 7; %h{1;2}.raku], '(7,)', 'a hash with several keys';
same Q[my @c = [[1, 2], [3, 4]]; @c[*;1].raku], '(2, 4)', 'a whatever dimension';
same Q[my @d = [[1, 2], [3, 4]]; @d[0;1] += 10; @d[0;1]], 12, 'a compound assignment';
same Q[my @e = [[1, 2], [3, 4]]; @e[0,1;1].raku], '(2, 4)', 'a list as a dimension';
same Q[our $gv = 7; my $n = "gv"; $::($n)], 7, 'a symbolic scalar';
same Q[our $gw; my $m = "gw"; $::($m) = 9; $gw], 9, 'a symbolic assignment';
same Q[my $t = "Int"; ::($t).^name], 'Int', 'a symbolic type lookup';
same Q[our @ga = 1, 2, 3; my $q = "ga"; @::($q).elems], 3, 'a symbolic array';
my $heredoc = ['my $x = 5;', 'my $a = qq:to/EOT/;', '  v=$x and {$x + 1}', '  EOT', '$a'].join("\n");
same $heredoc, "v=5 and 6\n", 'an interpolating heredoc';
my $plain = ['my $x = 5;', 'my $a = q:to/EOT/;', '  plain $x', '  EOT', '$a'].join("\n");
same $plain, "plain \$x\n", 'a plain heredoc';
same Q[my @a = [[1, 2], [3, 4]]; (@a[0;1]:exists, @a[5;5]:exists).join(",")], 'True,False', 'a multi-dimensional `:exists`';
same Q[my @z = 1, 2, 3; @z[].elems], 3, 'a zen slice of an array';
same Q[my %hz = a => 1; %hz{}.elems], 1, 'a zen slice of a hash';
same Q[my $h = {:x}; $h<x>.Str], 'True', 'a hash literal with a value-less colonpair';
