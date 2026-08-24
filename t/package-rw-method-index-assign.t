use Test;

plan 4;

class PathAccessor {
    method at($container, *@steps) is rw {
        my $root := $container;
        for @steps -> $step {
            $root := $root{$step};
        }
        return-rw $root;
    }
}

sub add-copy(\container, @path, $step, $value) {
    my $root = container.deepmap({ .clone });
    PathAccessor.at($root, |@path){$step} = $value;
    $root;
}

my %original = a => { b => 1 };
my %result = add-copy(%original, ["a"], "c", 2);

is-deeply %original, { a => { b => 1 } },
    "package rw accessor does not mutate the original hash";
is-deeply %result, { a => { b => 1, c => 2 } },
    "package rw accessor updates the copied hash";

my %array-original = a => [1];
my %array-result = add-copy(%array-original, ["a"], 1, 2);

is-deeply %array-original, { a => [1] },
    "package rw accessor does not mutate the original array";
is-deeply %array-result, { a => [1, 2] },
    "package rw accessor updates the copied array";
