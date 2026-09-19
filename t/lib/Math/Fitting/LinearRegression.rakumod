unit module Math::Fitting::LinearRegression;

use Math::Fitting::Predicates;

our sub probe-is-positional($object, $length) is export {
    is-positional-of-lists($object, $length)
}

our proto sub probe-fit(|) {*}
multi sub probe-fit(
    $data where is-positional-of-lists($data, 2),
    :p(:$prop) is copy = Whatever
) is export {
    "fit"
}
