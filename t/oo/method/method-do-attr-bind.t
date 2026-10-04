use Test;
use nqp;

# A Method's `$!do` is the body dispatch runs (ADR-11827 §2.4), and a method
# declaration's traits all see one Method object. Upstream NativeCall's
# `method m(...) is native` relies on both.

plan 8;

role Repl {
    has $!tag = 'replaced';
    method install() {
        my $replacement := -> |c { "$!tag {c.list.elems} {c.list[0].^name}" };
        nqp::bindattr(self, Code, '$!do', nqp::getattr($replacement, Code, '$!do'));
    }
}

{
    class K {
        method fill(Int $c, Int $n) { 'body' }
        method plain { 'plain' }
    }
    my $m = K.^find_method('fill');
    $m does Repl;
    $m.install;
    is K.new.fill(1, 2), 'replaced 3 K',
        'a bound $!do runs for the method, invocant first';
    is K.new.plain, 'plain', 'another method of the class keeps its body';
}

{
    class W { method greet { 'hi' } }
    my $m = W.^find_method('greet');
    $m does Repl;
    $m.install;
    $m.wrap(-> |c { "<" ~ callsame() ~ ">" });
    is W.new.greet, '<replaced 1 W>', 'a wrapper runs around the bound body';
}

{
    role A { }
    role B { }
    my @seen;
    multi trait_mod:<is>(Method $m, :$first!)  { $m does A }
    multi trait_mod:<is>(Method $m, :$second!) { @seen.push: $m ~~ A; $m does B }
    multi trait_mod:<is>(Method $m, :$third!)  { @seen.push: $m ~~ A && $m ~~ B }
    class T { method m() is first is second is third { } }
    is-deeply @seen, [True, True], 'every trait of a method sees the same Method object';
}

{
    my $returns;
    my $sig-returns;
    multi trait_mod:<is>(Method $m, :$probe!) {
        $returns = $m.returns;
        $sig-returns = $m.signature.returns;
    }
    class R { method m(--> Int) is probe { 1 } }
    ok $returns === Int, 'a method trait sees the declared return type';
    ok $sig-returns === Int, '... through its signature too';
}

{
    my $arity;
    multi trait_mod:<is>(Method $m, :$count!) { $arity = $m.signature.arity }
    class C { method m(C:D: Int $x) is count { 'body' } }
    is $arity, 2, 'a method trait signature counts the invocant';
    is C.new.m(1), 'body', 'a trait that binds nothing leaves the declared body';
}
