use Test;

# A role mixed into a user-defined metaclass (`$type.HOW does SomeRole`, what
# a trait does at declaration time) must not change what `$obj.^meth`
# passes the metaclass: a user-defined HOW receives the object itself, the
# instance for an instance (#18408).

plan 6;

role Extra { method extra(Mu \obj) { obj.DEFINITE } }

class MyHOW is Metamodel::ClassHOW {
    method touch(\obj) { obj.DEFINITE }
}

my $T := MyHOW.new_type(:name<T>);
$T.^add_method('via-meta', my method (\instance:) { instance.^touch });
$T.^compose;

ok $T.new.^touch, 'instance reaches the HOW before the mixin';
nok $T.^touch, 'type object reaches the HOW as itself';

$T.HOW does Extra;

ok $T.new.^touch, 'instance reaches the mixed-in HOW as the instance';
ok $T.new.via-meta, '.^ inside a method also passes the instance';
ok $T.new.^extra, 'a method of the mixed-in role receives the instance';
nok $T.^extra, 'the type object stays a type object';
