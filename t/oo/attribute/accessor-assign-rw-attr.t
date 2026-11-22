use Test;

# `$obj.attr = v` through a generated `is rw` accessor finds the one public
# attribute without cloning every attribute definition (#15079): the store,
# the inherited and redeclared cases, the attribute's own type check and the
# refusal of a read-only accessor are unchanged.

plan 10;

class Base {
    has int16 $.second is rw;
    has Str   $.label  is rw = 'base';
    has Int   $.fixed  = 3;
    has Int   $.typed;
}
class Derived is Base {
    has @.label is rw;
}

my $b = Base.new;
$b.second = 59;
is $b.second, 59, 'an is rw native attribute stores';
$b.second = 70000;
is $b.second, 4464, 'the int16 attribute wraps';
$b.label = 'x';
is $b.label, 'x', 'an is rw Str attribute stores';
throws-like { $b.label = 42 }, X::TypeCheck::Assignment, 'the attribute type is checked';
throws-like { $b.fixed = 4 }, Exception, 'a read-only accessor refuses the store';
throws-like { $b.typed = 4 }, Exception,
    message => /'Int'/, 'refusing an unset read-only attribute names its type';

my $d = Derived.new;
$d.second = 5;
is $d.second, 5, 'an inherited is rw attribute stores';
$d.label = <a b>;
is $d.label.join(','), 'a,b', 'the derived class redeclares the public accessor';
# The untyped `@.label` is the attribute: the base's `Str` does not type it
# (#17994).
is $d.label.raku, '["a", "b"]', 'the redeclared attribute is an untyped Array';
is Derived.new.label.raku, '[]', 'also as constructed';
