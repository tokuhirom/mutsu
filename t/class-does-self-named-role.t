use Test;

# A class whose own name collides with the name of a CORE role it composes.
# `class Iterator does Iterator` must compose the CORE `Iterator` role (a class
# cannot compose itself), NOT be treated as self-inheritance or a C3 self-cycle.
# Regression: mutsu resolved the `does Iterator` parent to the class being
# declared, giving "cannot inherit from itself" at top level and a "C3 MRO cycle"
# inside a package.

plan 6;

# --- top level: does the CORE Iterator role, same name as the class ---
{
    class Iterator does Iterator {
        has @.items;
        method pull-one {
            return IterationEnd unless @!items.elems;
            return @!items.shift;
        }
    }
    my $it = Iterator.new(items => [1, 2, 3]);
    is $it.^name, 'Iterator', 'self-named class keeps its own name';
    ok $it ~~ Iterator, 'instance is-a the (self-named) class';
    is $it.pull-one, 1, 'composed CORE Iterator role: pull-one works (1)';
    is $it.pull-one, 2, 'composed CORE Iterator role: pull-one works (2)';
}

# --- inside a package: same collision, previously a "C3 MRO cycle" ---
{
    my $ok = EVAL q:to/CODE/;
        package My::Pkg {
            class Iterator does Iterator {
                has @.items;
                method pull-one {
                    return IterationEnd unless @!items.elems;
                    return @!items.shift;
                }
            }
        }
        My::Pkg::Iterator.new(items => [7]).pull-one;
        CODE
    is $ok, 7, 'packaged self-named `does Iterator` composes the role (no C3 cycle)';
}

# --- genuine self-inheritance via `is` must still be rejected ---
{
    try { EVAL 'class SelfIs is SelfIs {}' }
    ok $!, 'real self-inheritance (`is`) is still an error';
}
