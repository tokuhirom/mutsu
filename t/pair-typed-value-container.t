use Test;

plan 7;

# A typed anonymous scalar (`my T $`) used as a Pair value carries its `of`-type
# constraint, so assigning a wrong-typed value through `.value` raises
# X::TypeCheck::Assignment. (roast S02-types/pair.t)

{
    my $p = Pair.new("foo", my Int $);
    isa-ok $p.value, Int, 'typed Pair value reports the constrained type';
    is ($p.value = 42), 42, 'assigning a matching value returns it';
    is $p.value, 42, 'the matching value was stored';
    throws-like { $p.value = "bar" }, X::TypeCheck::Assignment,
        'assigning a Str to an Int Pair value throws X::TypeCheck::Assignment';
    is $p.value, 42, 'the failed assignment left the value unchanged';
}

# The constraint is enforced for other types too.
{
    my $p = Pair.new("k", my Str $);
    $p.value = "ok";
    is $p.value, "ok", 'Str-constrained Pair value accepts a Str';
    throws-like { $p.value = 5 }, X::TypeCheck::Assignment,
        'assigning an Int to a Str Pair value throws X::TypeCheck::Assignment';
}
