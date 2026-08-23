use Test;

plan 2;

is {;}.signature.gist, '(;; $_? is raw = OUTER::<$_>)',
    'a bare block renders its implicit topic parameter';
is -> {}.signature.gist, '()',
    'an explicitly empty pointy block keeps an empty signature';
