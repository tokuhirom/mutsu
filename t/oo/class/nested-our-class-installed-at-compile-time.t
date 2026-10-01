use Test;

# An `our`-scoped type declared inside a routine or block is installed in
# its package at compile time, whether or not the code declaring it ever
# runs (#10470).

plan 12;

sub never-called { class NeverCalled { method m { 42 } } }
is NeverCalled.^name, 'NeverCalled', 'class in an uncalled sub is installed';
is NeverCalled.m, 42, 'its methods are callable';

sub never-called-role { role NeverCalledRole { method r { 7 } } }
class UsesRole does NeverCalledRole { }
is UsesRole.r, 7, 'role in an uncalled sub can be composed';

my @log;
sub with-body { class WithBody { @log.push('body') } }
my $before = WithBody;
is-deeply @log, [], 'the class body does not run at compile time';
with-body();
is-deeply @log, ['body'], 'the class body runs when the routine runs';
with-body();
is-deeply @log, ['body', 'body'], 'and on every entry of the routine';
ok $before === WithBody, 'the type object is the same before and after';

class Outer { method make { class Inner { method v { 'inner' } } } }
is Outer::Inner.v, 'inner', 'class in a method is installed in the class package';

module Mod { sub f { class InMod { } } }
is Mod::InMod.^name, 'Mod::InMod', 'class in a sub of a module is installed in the module';

if False { class InDeadBranch { } }
is InDeadBranch.^name, 'InDeadBranch', 'class in a branch that never runs is installed';

sub with-parent { class Child is Int { } }
ok Child.^mro.map(*.^name).first('Int'), 'parent of a nested class is known';

sub uses-own { class Own { method v { 3 } }; Own.v }
is uses-own(), 3, 'the in-place registration still works from inside the routine';
