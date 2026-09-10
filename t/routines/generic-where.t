use v6;
use Test;

# Generic .WHERE: every value answers .WHERE with an Int that is stable per
# identity (same object => same WHERE, distinct objects => distinct). mutsu
# derives it from the WHICH identity string since scalar values are unboxed
# (Rakudo reports the boxed object's memory address).

plan 20;

ok 42.WHERE ~~ Int, '42.WHERE is an Int';
ok 42.WHERE == 42.WHERE, 'same Int value, same WHERE';
nok 42.WHERE == 43.WHERE, 'different Int value, different WHERE';
ok "abc".WHERE ~~ Int, 'Str.WHERE is an Int';
ok 1.5.WHERE ~~ Int, 'Num.WHERE is an Int';
ok (1/3).WHERE ~~ Int, 'Rat.WHERE is an Int';
ok (2+3i).WHERE ~~ Int, 'Complex.WHERE is an Int';
ok True.WHERE ~~ Int, 'Bool.WHERE is an Int';
ok (1..5).WHERE ~~ Int, 'Range.WHERE is an Int';

my @a = 1, 2;
my @b = 1, 2;
ok @a.WHERE ~~ Int, 'Array.WHERE is an Int';
ok @a.WHERE == @a.WHERE, 'same array, same WHERE';
nok @a.WHERE == @b.WHERE, 'distinct arrays differ even with equal contents';

my %h = a => 1;
ok %h.WHERE ~~ Int, 'Hash.WHERE is an Int';

ok Int.WHERE ~~ Int, 'type object WHERE is an Int';
ok Any.WHERE ~~ Int, 'Any.WHERE is an Int';

class C { }
my $c = C.new;
my $d = C.new;
ok $c.WHERE == $c.WHERE, 'same instance, same WHERE';
nok $c.WHERE == $d.WHERE, 'distinct instances differ';

role R { }
ok R.WHERE ~~ Int, 'role type object WHERE is an Int';

enum E <A B>;
ok A.WHERE == A.WHERE, 'enum value WHERE is stable';

sub f() { 1 }
ok &f.WHERE ~~ Int, 'Sub.WHERE is an Int';
