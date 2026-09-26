use Test;

plan 1;

# A parameterised role's type parameter is private to the role method call.
# It must not overwrite a caller parameter with the same name when CallMethodMut
# reconciles the receiver after a method call.
role Typed[$type] {
    method type {
        my $captured = $type;
        True
    }
}

sub read-twice($value, $type where Typed) {
    my $first = $type.type;
    my $second = $type.type;
    $first && $second
}

is read-twice(1, Typed[Int]), True,
    'role type bindings do not overwrite a same-named constrained caller parameter';
