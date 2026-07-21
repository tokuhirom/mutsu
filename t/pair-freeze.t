use v6;
use Test;

plan 11;

# Pair.freeze returns the (decontainerized) value.
{
    my $str = "apple";
    my $p = Pair.new('key', $str);
    $p.value = "orange";
    is $p.value, "orange", 'value is assignable before freeze';
    is $p.freeze, "orange", 'freeze returns the current value';
    is $p.value, "orange", 'value is unchanged after freeze';
}

# After freeze, assigning the value raises X::Assignment::RO.
{
    my $p = Pair.new('key', 'apple');
    $p.freeze;
    throws-like { $p.value = "pear" }, X::Assignment::RO,
        'assigning a frozen Pair value dies with X::Assignment::RO';
    is $p.value, 'apple', 'frozen value keeps its original value';
}

# freeze severs the Scalar-container alias to an outer variable.
{
    my $v = 'value B';
    my $pair = a => $v;
    $pair.freeze;
    $v = 'value C';
    is $pair.value, 'value B', 'freeze severs the alias to the source variable';
    is ~$pair, "a\tvalue B", 'Str of a frozen pair reflects the frozen value';
}

# A typed container value keeps its type constraint through freeze.
{
    my $p = Pair.new("foo", my Int $);
    is ($p.value = 42), 42, 'can assign integer value and return it';
    throws-like { $p.value = "bar" }, X::TypeCheck::Assignment,
        'cannot assign a Str to an Int-typed pair value';
    $p.freeze;
    throws-like { $p.value = 666 }, X::Assignment::RO,
        'cannot assign to a frozen typed pair value';
    is $p.value, 42, 'typed value did not change';
}
