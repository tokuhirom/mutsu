use Test;

# raku-RandomColor uses this shape in RandomColor.BUILD: a writable `with`
# topic is nested inside a loop whose implicit topic is an immutable range item.
plan 1;

my $seed = 1;
for ^1 {
    with $seed {
        when Str {
            $_ = 'seed' ~ 1;
        }
        when Int | Num {
            $_ = $seed + 1;
        }
    }
}

is $seed, 2, 'with writable scalar topic overrides an outer readonly loop topic';
