use Test;

# Dynamic method calls and subscripts in RakuAST, measured on rakudo 2026.09:
#
# - `$o.$n(1)` is an `ApplyPostfix` whose postfix is `Call::TermAsMethod(callee =>
#   $n, args => ArgList(1))`; `.?` / `.*` is its `dispatch`;
# - `$o.&f(1)` is a `Call::NameAsMethod(name => f, args => ...)`;
# - `@a>>.$n()` wraps either in a `MetaPostfix::Hyper`.
#
# The round trip is the parsed program. The tree part of this file also passes
# under `raku`; the round trip part is mutsu's.

plan 28;

sub exprs($src) { ('my ($o, $n, $c); my @a; sub f(|) { }; ' ~ $src).AST.statements.skip(3).map(*.expression) }
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

# --- the round trip is the parsed program
same Q[my $n = -> $s { $s.uc }; "abc".$n], 'ABC', 'a method call by a callable';
same Q[sub f($x, $y) { $x ~ $y }; "a".&f("b")], 'ab', 'a routine called as a method';
same Q[my $k = -> $s, $t { $s ~ $t }; "a".$k("b")], 'ab', 'a callable with an argument';
same Q[my $n = -> $s { $s.uc }; my @a = <a b>; (@a>>.$n).join(",")], 'A,B', 'a hyper dynamic call';
same Q[sub f($x, $y) { $x ~ $y }; my @a = <a b>; (@a>>.&f("x")).join(",")], 'ax,bx', 'a hyper `.&f`';
same Q[class Foo { method bar() { "bar" } }; my $o = Foo.new; my $m = "bar"; $o."$m"()], 'bar', 'an interpolated quoted name still works';
