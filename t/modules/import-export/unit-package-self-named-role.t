use Test;

plan 1;

# A unit package qualifies the class name before registration. The bare role
# in `class Iterator does Iterator` must still resolve to CORE Iterator rather
# than the package-qualified class being declared.
EVAL q:to/CODE/;
    unit package Unit::SelfNamedIterator;
    class Iterator does Iterator {
        has @.items;
        method pull-one {
            return IterationEnd unless @!items.elems;
            return @!items.shift;
        }
    }
    CODE

is EVAL(q{ Unit::SelfNamedIterator::Iterator.new(items => [9]).pull-one }),
    9,
    'unit-package self-named `does Iterator` keeps the core role';
