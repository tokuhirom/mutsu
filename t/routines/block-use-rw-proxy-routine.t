use v6.*;
use Test;
use lib 't/lib';

# An `is rw` routine a block's `use` imports hands its Proxy on un-fetched:
# `with substr(...) { $_ = ... }` binds the topic to the Proxy and the store
# reaches the caller's variable, as with a by-name import (#14361).
plan 3;

{
    use P5substr;
    my $other = '1234';
    with substr($other, 1, 2) {
        $_ = 'a';
        is $other, '1a4', 'a family candidate returning a Proxy keeps it for the topic';
        $_ = 'xyz';
        is $other, '1xyz4', 'and a second store goes through the same Proxy';
    }
    my $copy = '1234';
    my $p = substr($copy, 1, 2);
    $p = 'q';
    is $copy, '1234', 'a Proxy assigned to a variable is a copy';
}
