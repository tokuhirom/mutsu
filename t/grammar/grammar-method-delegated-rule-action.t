use Test;

# A grammar method that delegates to another rule returns that rule's Match,
# so the delegate rule's own action fires on the capture (#17179).

plan 8;

grammar G {
    token TOP { 'a' <x=.key('thing')> }
    method key($n) { self."$n"() }
    token thing { 'b' }
}
class A {
    method thing($/) { make "thing-made" }
    method TOP($/) { make "x=" ~ ($<x>.made // 'none') }
}
is G.parse('ab', :actions(A.new)).made, 'x=thing-made', 'aliased capture through a method fires the delegate action';

# The delegate's own sub-captures stay on the Match the action sees.
grammar H {
    token TOP { <y=.key('pair')> }
    method key($n) { self."$n"() }
    token pair { <k=.word> '=' <v=.word> }
    token word { \w+ }
}
class B {
    method word($/) { make $/.Str.uc }
    method pair($/) { make "{$<k>.made}:{$<v>.made}" }
    method TOP($/) { make $<y>.made }
}
is H.parse('ab=cd', :actions(B.new)).made, 'AB:CD', 'delegate action sees its sub-captures';

# Without a method in between (control).
grammar C {
    token TOP { 'a' <x=.thing> }
    token thing { 'b' }
}
is C.parse('ab', :actions(A.new)).made, 'x=thing-made', 'direct aliased call (control)';

# A method that matches without delegating does not borrow a stale action.
grammar D {
    token TOP { <thing> <x=.plain> }
    token thing { 'a' }
    method plain() { self.sparse }
    token sparse { 'a' }
}
class E {
    has @.log;
    method thing($/) { @!log.push('thing') }
    method sparse($/) { @!log.push('sparse') }
}
my $e = E.new;
D.parse('aa', :actions($e));
is $e.log.join(','), 'thing,sparse', 'each delegate fires its own action once';

# Silent call through a method still fires the delegate's action.
grammar F {
    token TOP { <.key('thing')> }
    method key($n) { self."$n"() }
    token thing { 'b' }
}
my $f = E.new;
class SilentA { has @.log; method thing($/) { @!log.push('thing') } }
my $sa = SilentA.new;
F.parse('b', :actions($sa));
is $sa.log.join(','), 'thing', 'silent call through a method fires the delegate action';

# Plain method (no delegation) still works.
grammar P {
    token TOP { 'a' <x=.m> }
    method m() { self.tok }
    token tok { 'b' }
}
ok P.parse('ab'), 'method-delegated rule matches without actions';
is P.parse('ab')<x>.Str, 'b', 'aliased capture text';
is P.parse('ab')<x>.from, 1, 'aliased capture position';

done-testing;
