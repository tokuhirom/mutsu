use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 5;

# Basic group-of with passing tests
group-of 2 => 'basic group' => {
    ok True, 'first test';
    ok True, 'second test';
}

# group-of with is checks
group-of 3 => 'is checks' => {
    is 1 + 1, 2, 'addition';
    is "hello".chars, 5, 'string length';
    is (1, 2, 3).elems, 3, 'list length';
}

# group-of with a single test
group-of 1 => 'single test' => {
    ok 42 > 0, 'positive number';
}

# group-of with mixed test functions
group-of 2 => 'mixed tests' => {
    ok True, 'truthy value';
    is 'hello', 'hello', 'string equality';
}

# Nested group-of (group-of inside group-of)
group-of 1 => 'outer group' => {
    group-of 1 => 'inner group' => {
        ok True, 'nested test';
    }
}
