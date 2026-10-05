use Test;

plan 12;

is (1 + 2i).re, 1, 'Complex.re returns the real component';
is (1 + 2i).im, 2, 'Complex.im returns the imaginary component';

for 7, 1.5, (1 / 2), True, '6+8i' -> $value {
    throws-like { $value.re }, X::Method::NotFound,
        "$value does not provide .re";
    throws-like { $value.im }, X::Method::NotFound,
        "$value does not provide .im";
}
