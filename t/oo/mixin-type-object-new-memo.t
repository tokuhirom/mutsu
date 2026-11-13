use Test;

# `.new` on a role-mixed type object (`Base but R`) takes the default
# constructor when nothing claims `new`; #17331 remembers that verdict per
# type object, so a later `new` must still be seen.

plan 6;

role Mark { has $.mark = 'm' }
class Base { has $.v }

my $T = Base but Mark;
for ^2 {
    my $o = $T.new(:v(1));
    is $o.v, 1, 'the default constructor binds the attribute';
    is $o.mark, 'm', 'and the role default';
}

Base.^add_method('new', method (|) { 'custom' });
is $T.new(:v(2)), 'custom', 'a new added later claims the call';

role Ctor { method new(|) { 'from role' } }
my $U = Base but Ctor;
is $U.new, 'from role', 'a role with its own new claims it';
