use Test;
use nqp;

# annotations::containers' `Binder.STORE` chooses between assigning and
# binding with `nqp::isrwcont($!value)`. After `$!value := $topic` with a
# variable, the attribute holds that variable's container and is writable;
# only a bound literal leaves it without one. mutsu marked every `:=` of a
# private attribute as a bound value, so the check was always False.

plan 6;

class A {
    has $!value is default(Nil);
    method bind(Mu $topic is raw) { $!value := $topic; nqp::isrwcont($!value) }
    method bind-var { my $x = 3; $!value := $x; nqp::isrwcont($!value) }
    method bind-lit { $!value := 3; nqp::isrwcont($!value) }
    method fresh { nqp::isrwcont($!value) }
    method bind-then-assign(Mu $topic is raw, $new) { $!value := $topic; $!value = $new; $!value }
}

my $item = 1;
is A.new.bind($item), 1, 'a raw parameter bound to a variable stays writable';
is A.new.bind(($ = 1)), 1, 'so does an anonymous scalar';
is A.new.bind-var, 1, 'a lexical variable';
is A.new.bind-lit, 0, 'a literal leaves no container';
is A.new.fresh, 1, 'an unbound attribute is a container';
A.new.bind-then-assign($item, 5);
is $item, 5, 'assigning through the attribute writes the bound variable';
