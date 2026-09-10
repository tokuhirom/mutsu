use Test;

plan 6;

# A `&foo` parameter shadows a same-named package sub *even after* the package
# sub has already been called (which previously primed the name-keyed call cache
# and made the cache wrongly win over the lexical parameter).
{
    sub foo($x) { $x + 1 }
    sub callit(&foo) { foo(1) }

    is foo(1), 2, 'package foo called first (primes the call cache)';
    is callit({ $^x + 2 }), 3, '&foo parameter shadows package foo inside callit';
    is foo(1), 2, 'package foo still resolves correctly outside callit';
}

# Same for an `&infix:<op>` operator parameter shadowing a package operator.
{
    sub infix:<@@> ($x, $y) { $x + $y }
    sub op2(&infix:<@@>) { 2 @@ 3 }

    is 2 @@ 3, 5, 'package operator called first';
    is op2({ $^a * $^b }), 6, '&infix:<@@> parameter shadows the package operator';
    is 2 @@ 3, 5, 'package operator still resolves outside op2';
}
