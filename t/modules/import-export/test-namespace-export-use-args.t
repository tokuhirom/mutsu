use Test;

plan 2;

# Regression from the Test::When distribution: `use Test::When <smoke>`
# compiled through the `Test`/`Test::*` special case, which dropped the
# `use` arguments, so its `sub EXPORT` always saw an empty list.

use lib 't/lib';

{
    use Test::UseArgsFixture <smoke online>;
    is test-ns-export-args(), 'smoke,online',
        'sub EXPORT of a Test:: module receives positional use arguments';
}

{
    use Test::UseArgsFixture 'author', 'release';
    is test-ns-export-args(), 'author,release',
        'sub EXPORT of a Test:: module receives a comma list of use arguments';
}
