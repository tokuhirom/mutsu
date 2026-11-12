use Test;

# `$obj.^m` is `$obj.HOW.m($obj)`: on a type whose metaobject is a user
# metaclass that declares `m`, the metaclass's own method runs with the
# receiver as its argument, from the type object and from an instance alike.
# Red's `model` declarator makes such types, and `Red::Column.gist` calls
# `$model.^as` for every column it renders.

plan 7;

my $count = 0;
class MyHOW is Metamodel::ClassHOW {
    method as(Mu \type) { $count++; "as:" ~ type.^name }
    method tag(Mu $t) { "tag" }
}
my $t := MyHOW.new_type(:name<T>);
$t.^compose;

is $t.^as, 'as:T', 'a metaclass method through .^ on the type object';
is $t.^as, 'as:T', 'again';
is $count, 2, 'the method ran once per call';
is $t.^tag, 'tag', 'another metaclass method';
my $o = $t.new;
is $o.^as, 'as:T', 'from an instance';
is $count, 3, 'which runs the same method';
nok $t.^can('as'), 'a metaclass method is not a method of the type';
