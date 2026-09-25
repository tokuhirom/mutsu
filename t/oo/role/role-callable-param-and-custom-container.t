use Test;

# Regression reduced from Concurrent::PriorityQueue 0.0.2.
plan 3;

multi sub record(
    \keys,
    Int:D \key,
    \values,
    Str:D \value,
    :&cmp!,
    :$force!,
) {
    keys.push(key);
    values.push(value);
    cmp(key, key);
    $force;
}

role Queue[&cmp] {
    has Int @!keys;
    has Str @!values;

    method STORE(*@values, *%_) {
        @!keys = @values;
    }

    method add(Int:D \key, Str:D \value) {
        record @!keys, key, @!values, value, :&cmp, :force;
    }

    multi method push(Int:D \value) {
        @!keys.push(value);
    }
}

class C does Queue[&infix:<cmp>] { }

my $queue = C.new;
is $queue.add(42, "answer"), True,
    'role callable parameter forwards as a named code argument';

my @q is C = 1;
my &bad-push = { @q.push("bad") };
dies-ok { bad-push() },
    'captured custom positional container keeps its method dispatch';
is @q.^name, 'C', 'custom positional container remains bound to its class';
