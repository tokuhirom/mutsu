use Test;

# A `my class` inside a routine is one type object whose shape is fixed at
# compile time; only its body statements and its methods' closure over the
# current frame are per entry (#16978). mutsu keeps such a class across entries
# instead of rebuilding it, so these pin what must still be per entry.

plan 12;

sub plain { my class C { method m { 42 } }; C }
ok plain() === plain(), 'the same type object on every entry';
is plain().m, 42, 'its methods still dispatch';

sub closes($x) { my class D { method x { $x } }; D.x }
is closes(1), 1, 'a method sees the first entry';
is closes(2), 2, 'a method sees a later entry';

sub attr-default($x) { my class E { has $.a = $x }; E.new.a }
is attr-default(5), 5, 'an attribute default sees the first entry';
is attr-default(6), 6, 'an attribute default sees a later entry';

my @ran;
sub with-body($x) { my class F { @ran.push($x); method m { 1 } }; F.m }
with-body(1); with-body(2);
is-deeply @ran, [1, 2], 'a class body with statements runs on every entry';

class Base { method base { 'base' } }
role R { method r { 'r' } }
sub derived { my class G is Base does R { method own { 'own' } }; G }
is derived().new.base ~ derived().new.r ~ derived().new.own, 'baserown',
    'parents and roles survive re-entry';

sub mop { my class H { method m { 1 } }; H }
mop().^add_method('extra', method { 'x' });
ok mop().^can('extra'), 'a method added through the MOP stays, as on rakudo';
is mop().extra, 'x', 'and it dispatches on a later entry';

sub lexical-name { my class Named { }; Named.^name }
is lexical-name(), 'Named', 'the name is the declared one';
is lexical-name(), 'Named', 'on a later entry too';
