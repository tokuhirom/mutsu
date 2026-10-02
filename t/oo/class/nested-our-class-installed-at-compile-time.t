use Test;

# An `our`-scoped type declared inside a routine or block is installed in
# its package at compile time, whether or not the code declaring it ever
# runs (#10470).

plan 17;

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

# The nested class composes a role declared earlier at unit level, and the
# role's body runs once, at compile time, not again on each entry of the
# routine (#10494).
our $counted-role-runs;
role CountedRole { $counted-role-runs++; method c { 'c' } }
sub declares-counted { class CountedClass does CountedRole { } }
is $counted-role-runs, 1, 'the role body ran at compile time';
is CountedClass.c, 'c', 'the uncalled routine\'s class composed the earlier role';
declares-counted() for ^2;
is $counted-role-runs, 1, 'the role body runs once however often the routine runs';

# A parameterized role re-registered on each routine entry closes over that
# entry's lexicals.
sub make-param-reader($x) {
    my $captured = $x;
    role EntryReader[::T] { method value() { $captured } }
    class EntryComposed does EntryReader[Int] { }
    EntryComposed.new;
}
is make-param-reader(1).value, 1, 'parameterized role method sees the first entry';
is make-param-reader(2).value, 2, 'and the second entry';
