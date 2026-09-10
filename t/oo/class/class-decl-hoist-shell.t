use v6;
use Test;

plan 8;

# Raku type declarations are compile-time: a mainline statement that runs
# before the textual class declaration can already construct the type
# (pre-registered as a declaration-only shell; see hoist_type_decl_shells).
# Real-world shape: Cro::HTTP::Router's
#   our $link-plugin = router-plugin-register('link');
# at the top of the module body, with `class PluginKey` declared further down.

# Forward construction at file scope.
our $k1 = mk1();
class FwdK1 { has $.v }
sub mk1 { FwdK1.new(v => 9) }
is $k1.v, 9, 'mainline before class can construct it via a sub';

# Forward construction inside a module body (the Cro::HTTP::Router shape).
module Fwd::M {
    our $k = mk('link');
    class PluginKey { has Str $.id is required }
    sub mk(Str $id) { Fwd::M::PluginKey.new(:$id) }
}
is $Fwd::M::k.id, 'link', 'module-body mainline before nested class (FQ .new)';

# Forward method call.
our $m1 = fwd-method();
class FwdK2 { method greet { 'hi' } }
sub fwd-method { FwdK2.greet }
is $m1, 'hi', 'forward reference can call a method';

# Class body side effects still run at their textual position.
my @order;
@order.push('before');
class SideEffect { @order.push('body') }
@order.push('after');
is @order.join(','), 'before,body,after',
    'class body statements still execute in mainline order';

# A method closing over an earlier lexical still sees its runtime value.
my $x = 5;
class CapturesLex { method m { $x } }
is CapturesLex.m, 5, 'method closes over earlier lexical';

# A forward-constructed class composing a (textually earlier) role.
role FwdRole { method role-m { 'from-role' } }
our $r1 = fwd-role();
class FwdDoes does FwdRole { }
sub fwd-role { FwdDoes.new.role-m }
is $r1, 'from-role', 'forward reference through role composition';

# A nested grammar's implicit Grammar parent must not resolve to itself
# (regression guard: the shell of a single-statement class body must not
# pre-insert the short name and self-inherit).
class NestedGrammarHost {
    grammar Grammar {
        token TOP { \w+ }
    }
    method hit(Str $s) { so Grammar.parse($s) }
}
ok NestedGrammarHost.hit('abc'), 'nested grammar named Grammar parses';

# Inheritance between hoisted classes keeps textual order.
our $b1 = mkb();
class BaseA { method who { 'BaseA' } }
class ChildB is BaseA { }
sub mkb { ChildB.who }
is $b1, 'BaseA', 'forward reference to a class with a hoisted parent';
