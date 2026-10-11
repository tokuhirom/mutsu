use Test;

# `my $x := $!t` binds the attribute's own container on its invocant. It must
# not leak to the code a callee runs on another invocant: a closure there reads
# `$!t` of ITS `self` (#12478). Every expectation was checked against rakudo.

plan 6;

class Callee {
    has $.t;
    method in-closure { (1,).map(-> $n { "$!t" }).join }
    method bind-in-closure { (1,).map(-> $n { my $pre = $!t; my $b := $!t; "$pre $b" }).join }
    method plain { "$!t" }
}

class Caller {
    has $.t;
    method call($o, $m) { my $q := $!t; $o."$m"() }
}

my $caller = Caller.new(t => 'Z');
is $caller.call(Callee.new(t => 'Y'), 'in-closure'), 'Y', 'a closure in the callee reads its own invocant\'s attribute';
is $caller.call(Callee.new(t => 'Y'), 'bind-in-closure'), 'Y Y', 'and binds its own invocant\'s attribute';
is $caller.call(Callee.new(t => 'Y'), 'plain'), 'Y', 'the callee\'s method body reads its own attribute';

class Writer {
    has $.t is rw;
    method via-closure { (1,).map(-> $n { my $b := $!t; $b = 'W' }).eager; $!t }
    method via-body { my $b := $!t; $b = 'V'; $!t }
}
is Writer.new(t => 'A').via-closure, 'W', 'a write through the bound name reaches the attribute from a closure';
is Writer.new(t => 'A').via-body, 'V', 'and from the method body';

my $w = Writer.new(t => 'A');
$w.via-body;
is $w.t, 'V', 'the attribute keeps the value written through the alias';

done-testing;
