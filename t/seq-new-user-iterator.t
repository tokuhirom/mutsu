use Test;

# A stateful user-defined `Iterator` whose `pull-one` transforms/generates
# elements. `Seq.new($it)` / `*.from-iterator($it)` must actually drive
# `pull-one` (deferred reification), not read an empty backing vec or wrap the
# iterator as a single element.

plan 16;

class Counter does Iterator {
    has $.n = 0;
    method pull-one() {
        return IterationEnd if $.n >= 3;
        $!n = $.n + 1;
        $.n;
    }
}

is Seq.new(Counter.new).List,  (1, 2, 3),  'Seq.new(user iter).List pulls';
is Seq.new(Counter.new).elems, 3,          'Seq.new(user iter).elems pulls';
is Seq.new(Counter.new).Array, [1, 2, 3],  'Seq.new(user iter).Array pulls';
is Seq.new(Counter.new).eager, (1, 2, 3),  'Seq.new(user iter).eager pulls';

my $c = 0;
for Seq.new(Counter.new) { $c++ }
is $c, 3, 'for Seq.new(user iter) iterates';

my @a = Seq.new(Counter.new);
is @a, [1, 2, 3], '@a = Seq.new(user iter) reifies';

is (|Seq.new(Counter.new)).elems, 3, 'slip Seq.new(user iter) expands';

is List.from-iterator(Counter.new),  (1, 2, 3), 'List.from-iterator(user iter)';
is Array.from-iterator(Counter.new), [1, 2, 3], 'Array.from-iterator(user iter)';
is Slip.from-iterator(Counter.new).List, (1, 2, 3), 'Slip.from-iterator(user iter)';

# A pull-one that transforms elements — the seq is NOT the raw attribute.
class Doubler does Iterator {
    has @.src;
    has $.i = 0;
    method pull-one() {
        return IterationEnd if $.i >= @.src.elems;
        my $v = @.src[$.i] * 2;
        $!i = $.i + 1;
        $v;
    }
}
is Seq.new(Doubler.new(src => (1, 2, 3))).List, (2, 4, 6),
    'Seq.new transforms via pull-one, not raw items';

# The built-in `.iterator` still reifies correctly (no infinite pull loop).
is Seq.new((1, 2, 3).iterator).List, (1, 2, 3), 'Seq.new(built-in iterator).List';

# `.sort`/`.reverse` (comparator-capable methods) also reify: chained (a rvalue
# receiver -> CallMethod) and via a variable (an lvalue receiver -> CallMethodMut).
is Seq.new(Counter.new).sort,        (1, 2, 3), 'chained .sort reifies';
is Seq.new(Counter.new).reverse,     (3, 2, 1), 'chained .reverse reifies';
my $s = Seq.new(Counter.new);
is $s.sort, (1, 2, 3), 'variable-target .sort reifies';

# A two-phase `pull-one { with $!k {...} else { $!k := $it.pull-one } }` iterator
# (the Hash::Agnostic `.kv` shape): the `else` branch's trailing `:=` bind must be
# the block's value, so the pulled key is returned; the next call emits its value.
class KV does Iterator {
    has $.backend;
    has $.iterator;
    has $!key;
    method pull-one() is raw {
        with $!key {
            my $key = $!key;
            $!key  := Mu;
            $!backend{$key};
        }
        else {
            $!key := $!iterator.pull-one;
        }
    }
}
my %b = a => 1, b => 2;
is Seq.new(KV.new(:backend(%b), :iterator(<a b>.iterator))).List, ("a", 1, "b", 2),
    'two-phase KV iterator (trailing := in with/else is the block value)';
