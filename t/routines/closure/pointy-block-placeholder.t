use Test;

plan 7;

# A pointy block ALWAYS has an explicit signature — even `-> { … }`, which
# declares zero parameters — so a placeholder written directly in its body
# cannot become its parameter the way it would in a bare `{ … }` block. Rakudo
# rejects that at compile time; mutsu used to defer it to a `Die` emitted where
# the closure literal is evaluated, so a pointy block buried inside a routine
# that is never called never reported it at all.

throws-like '-> { $^a }.()', X::Signature::Placeholder,
    'a placeholder in `-> { }` is rejected';
throws-like 'sub () { -> { $^a }.() }', X::Signature::Placeholder,
    '...even when the pointy block is never evaluated';
throws-like '-> $x { @_ }.(1)', X::Signature::Placeholder,
    'the implicit slurpy @_ counts too';
throws-like '-> $x { %_ }.(1)', X::Signature::Placeholder,
    'and %_';

# A bare block owns its placeholders, so it stays legal.
{
    my $f = { $^a + $^b };
    is $f(1, 2), 3, 'a bare block still collects its own placeholders';
}

# A nested bare block owns the placeholders written inside it, even when the
# enclosing pointy block has a signature.
is (-> $b, $i { ({ $^a + $^b }, { $^a * $^b })[$i](2, 3) }).(0, 1), 6,
    'a nested bare block claims its own placeholders';

# An explicitly declared @_ is a parameter, not a placeholder.
is (-> @_ { @_.elems }).([1, 2, 3]), 3, '`-> @_ { @_ }` stays legal';
