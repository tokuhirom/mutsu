use v6;
use Test;

# Bool `does Int`, so it unboxes to its integer value (True=1, False=0) wherever
# a native integer slot is stored to or bound. A prior fix only handled scalar
# assignment; this generalizes the unbox to native-int *array* elements and to
# *parameter binding* (code-review follow-up to the T-035 scalar fix).

plan 9;

# Scalar assignment (the original fix).
{
    my int $x = (3 >= 2);
    is $x, 1, 'True to native int scalar -> 1';
    is $x.^name, 'Int', 'native int scalar holds an Int';
    $x = (1 >= 2);
    is $x, 0, 'False to native int scalar -> 0';
}

# Sized native int scalar.
{
    my int8 $y = True;
    is $y, 1, 'True to native int8 -> 1';
}

# Native int array elements.
{
    my int @a = (3 >= 2), (1 >= 2);
    is @a, [1, 0], 'Bool elements in a native int array -> 1/0';
    is @a[0].^name, 'Int', 'native int array element is an Int';
}

# Parameter binding.
{
    sub f(int $x) { $x }
    is f(True), 1, 'True bound to a native int parameter -> 1';
    is f(False), 0, 'False bound to a native int parameter -> 0';
    is f(True).^name, 'Int', 'bound native int parameter is an Int';
}
