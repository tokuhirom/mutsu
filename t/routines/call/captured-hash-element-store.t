use Test;

# `%h{$k} = $v` from a `my sub` or closure whose `%h` is a free variable of the
# enclosing routine (#18055). The store lands in place in the captured hash;
# every shape the general store handles differently still behaves as before.
# Each store runs often enough to be on the hot path.

plan 18;

# The memo shape from MCP::Server::Tool::Web's tokenizer.
sub memo-run(Int $n) {
    my %memo;
    my sub put(Str $k, Int $i) { %memo{$k} = [$i, $i + 1] }
    put($_ %% 2 ?? 'even' !! 'odd', $_) for ^$n;
    %memo
}
my %m = memo-run(100);
is-deeply %m<even>, [98, 99], 'the last store of each key is visible to the owner';
is-deeply %m<odd>, [99, 100], '... for every key';
is %m.elems, 2, 'no stray keys';

# The value of the store expression is the itemized rvalue.
sub store-value() {
    my %h;
    my sub s() { %h<k> = (1, 2) }
    my $r;
    $r = s() for ^50;
    ($r.raku, %h<k>.raku)
}
my ($rv, $stored) = store-value();
is $rv, '$(1, 2)', 'the store expression yields the itemized value';
is $stored, '$(1, 2)', 'the element holds the itemized value';

# A closure capturing the hash.
sub closure-store() {
    my %h;
    my &f = -> $i { %h{"k$i"} = $i };
    f($_) for ^60;
    %h.elems
}
is closure-store(), 60, 'a closure stores into the captured hash';

# An element bound to a variable is written through.
sub bound-element() {
    my %h;
    my $target = 0;
    %h<b> := $target;
    my sub s($v) { %h<b> = $v }
    s($_) for ^50;
    ($target, %h<b>)
}
is-deeply bound-element(), (49, 49), 'a bound element writes through to its variable';

# Another holder of the same hash sees the store.
sub alias-holder() {
    my %h;
    my $alias = %h;
    my sub s($i) { %h{$i} = $i }
    s($_) for ^50;
    $alias.elems
}
is alias-holder(), 50, 'an item reference to the hash sees every store';

# A copy does not.
sub copy-holder() {
    my %h = a => 1;
    my %copy = %h;
    my sub s($i) { %h{$i} = $i }
    s($_) for ^50;
    (%copy.elems, %h.elems)
}
is-deeply copy-holder(), (1, 51), 'a copy taken before the stores keeps its own content';

# A typed hash still checks its values.
sub typed() {
    my Int %h;
    my sub s($v) { %h<k> = $v }
    s($_) for ^50;
    s('nope');
}
throws-like { typed() }, X::TypeCheck::Assignment, 'a typed hash still type-checks';

# `is default` still answers for a Nil store.
sub defaulted() {
    my %h is default(42);
    my sub s($v) { %h<k> = $v }
    s($_) for ^50;
    s(Nil);
    %h<k>
}
is defaulted(), 42, 'a Nil store into an `is default` hash gives the default';

# An object hash keeps its keys' identity.
sub object-hash() {
    my %h{Any};
    my sub s($k) { %h{$k} = 1 }
    s($_) for ^50;
    %h.keys.map(*.^name).unique.join
}
is object-hash(), 'Int', 'an object hash keeps non-string keys';

# A Map is immutable.
sub map-store() {
    my %h is Map = a => 1;
    my sub s() { %h<b> = 2 }
    s();
}
dies-ok { map-store() }, 'a Map refuses the store';

# Int keys are stringified.
sub int-keys() {
    my %h;
    my sub s($i) { %h{$i} = $i * 2 }
    s($_) for ^50;
    %h<7>
}
is int-keys(), 14, 'an Int key';

# Nested routines two levels deep.
sub two-levels() {
    my %h;
    my sub outer-sub($i) {
        my sub inner-sub() { %h{$i} = $i }
        inner-sub();
    }
    outer-sub($_) for ^50;
    %h.elems
}
is two-levels(), 50, 'a store two lexical levels down';

# Overwriting an existing key replaces the value.
sub overwrite() {
    my %h;
    my sub s($v) { %h<k> = $v }
    s($_) for ^50;
    %h<k>
}
is overwrite(), 49, 'overwriting a key keeps the latest value';

# A store whose value is the hash's own element keeps the element's value.
sub self-copy() {
    my %h = a => [1, 2];
    my sub s() { %h<b> = %h<a> }
    s() for ^50;
    %h<b>.push(3);
    (%h<a>.elems, %h<b>.elems)
}
is-deeply self-copy(), (3, 3), 'storing one element into another shares the Array, as in raku';

# Hash-valued element.
sub hash-value() {
    my %h;
    my sub s($i) { %h<inner> = { n => $i } }
    s($_) for ^50;
    %h<inner><n>
}
is hash-value(), 49, 'a Hash value';
