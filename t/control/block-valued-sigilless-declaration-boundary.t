use v6.d;
use Test;

# Implementation::Loader 0.0.10 exposed this parser boundary: a sigilless
# declaration whose value is a `do { ... }` block must end before the next-line
# conditional, so the conditional can see the declaration.
my \loaded = do { 42 }
unless loaded == 42 {
    die 'the declaration was incorrectly parsed into the conditional';
}

is loaded, 42, 'a block-valued sigilless declaration survives the next-line conditional';
done-testing;
