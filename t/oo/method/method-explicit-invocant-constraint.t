use Test;

# A method with an explicit invocant constraint (`::?CLASS:D:`, `Mu \SELF:`,
# `C:D:`) used to be kept off the compiled-method fast lane. The constraints
# that the receiver trivially meets now take it (perf); everything else still
# goes through the full binder and keeps its errors.
plan 19;

my class Base {
    has $.v = 7;
    method a(::?CLASS:D: $x) { $x + $!v }
    method b(Mu \SELF: $x) { SELF.v + $x }
    method c(Base:D: $x) { self.v * $x }
    method d(Base $self: $x) { $self.v - $x }
    method e(Mu $self: $x) { $self.v ~ $x }
    method f(::?CLASS $: $x) { $x }
    method g(Base:U: $x) { "type $x" }
    method h(Base:D $: $x) { "d $x" }
}
my class Sub is Base { has $.w = 100 }

my $b = Base.new;
is $b.a(1), 8, '::?CLASS:D: invocant';
is $b.b(1), 8, 'Mu \SELF: invocant reads SELF';
is $b.c(2), 14, 'Base:D: invocant';
is $b.d(2), 5, 'named invocant with a class constraint';
is $b.e(2), '72', 'Mu $self: invocant';
is $b.f(3), 3, '::?CLASS $: invocant';
is $b.h(4), 'd 4', 'Base:D $: invocant';
for ^3 { $b.a($_) }
is $b.a(2), 9, 'repeated calls stay correct';

my $s = Sub.new(v => 1);
is $s.a(1), 2, 'a subclass instance meets ::?CLASS:D:';
is $s.b(1), 2, 'a subclass instance meets Mu \SELF:';
is $s.c(5), 5, 'a subclass instance meets Base:D:';

# The checks the full binder raises are still raised.
throws-like { Base.a(1) }, X::Parameter::InvalidConcreteness, ':D invocant refuses a type object';
throws-like { Base.c(1) }, X::Parameter::InvalidConcreteness, 'Base:D: refuses a type object';
is Base.g(1), 'type 1', ':U invocant accepts a type object';
throws-like { $b.g(1) }, X::Parameter::InvalidConcreteness, ':U invocant refuses an instance';

# A method called through a differently classed receiver is still checked.
my class Other { has $.v = 1 }
throws-like { Base.^find_method('c')(Other.new, 1) }, X::TypeCheck::Binding::Parameter,
    'a foreign receiver fails the invocant check';

# Attribute and invocant reads inside a role method.
my role R { method r(::?CLASS:D: $x) { $x * 2 } }
my class WithR does R { }
is WithR.new.r(4), 8, 'a role method with a ::?CLASS:D: invocant';

# A rw / raw invocant keeps its container semantics.
my class Cnt {
    has $.n is rw = 0;
    method bump(Cnt:D: $by) { $!n += $by; self }
}
my $c = Cnt.new;
$c.bump(2).bump(3);
is $c.n, 5, 'a mutating method with a Cnt:D: invocant';
is Cnt.new.bump(1).n, 1, 'and on a fresh instance';
