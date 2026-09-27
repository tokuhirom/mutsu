use v6;
use Test;

plan 7;

# Rakudo compiles `for EXPR` to `EXPR.map(&body, :item(iscont(EXPR)))`, and
# `Any.map` iterates `SELF.iterator` unless `:item` is set. So a bare object --
# a method-call result, not a `$` container -- is iterated through its class's
# own `iterator` override even when the class does not compose `Iterable`.
# Found through Game::Entities, whose `View` class (a plain `my class` with
# `method iterator { $!view.iterator }`) is iterated with `for E.view(...)`.

class View {
    has $!view is built;
    method iterator { $!view.iterator }
}

sub make-view { View.new(view => (1 => 'a', 2 => 'b')) }

{
    my @keys;
    for make-view() { @keys.push: .key }
    is-deeply @keys, [1, 2], 'a sub-call result is iterated through its iterator';
}

{
    my @keys;
    for View.new(view => (3 => 'c',)) { @keys.push: .key }
    is-deeply @keys, [3], 'a method-call result is iterated through its iterator';
}

{
    my @values;
    for make-view() -> (:$key, :$value) { @values.push: "$key=$value" }
    is-deeply @values, ['1=a', '2=b'], 'the elements destructure in the loop signature';
}

{
    my $v = make-view();
    my @seen;
    for $v { @seen.push: .^name }
    is-deeply @seen, ['View'], 'a $-container source stays a single item';
}

{
    my \v = make-view();
    my @keys;
    for v { @keys.push: .key }
    is-deeply @keys, [1, 2], 'a sigilless binding is not a container, so it iterates';
}

{
    class Plain { }
    my @seen;
    for Plain.new { @seen.push: .^name }
    is-deeply @seen, ['Plain'], 'a class without an iterator override is one item';
}

{
    class Counted does Iterable {
        method iterator { (7, 8).iterator }
    }
    my @seen;
    for Counted.new { @seen.push: $_ }
    is-deeply @seen, [7, 8], 'an Iterable class still iterates through its iterator';
}
