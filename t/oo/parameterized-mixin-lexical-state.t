use Test;

plan 3;

class ParametricMixin {
    my role R[\value] {
        my \local = value;
        method value { value }
        method local { local }
    }

    method ^parameterize(\type, \value) {
        type.^mixin(R[value])
    }
}

my $object = ParametricMixin[42].new;
is $object.^name, 'ParametricMixin+{ParametricMixin::R[Int]}',
    'custom parameterization keeps the role argument in the composed type';
is $object.value, 42, 'the role method sees its parameter value';
is $object.local, 42, 'role-body lexical state is bound to the same parameter';
