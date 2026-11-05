use Test;

plan 7;

# `my %*ENV` declared in a block is the hash the element readers see (#16681).
{
    my %*ENV = A => 'x';
    %*ENV<C> = 'c';
    is %*ENV<C>, 'c', 'an element stored into the declared hash reads back';
    is %*ENV<A>, 'x', 'an element it was declared with';
    is %*ENV.keys.sort.join(','), 'A,C', 'its keys';
    is %*ENV{'A'}, 'x', 'a {} subscript';
    ok %*ENV<A>:exists, 'exists on the declared hash';
}

{
    my %*FOO = A => 'x';
    sub s2 { %*FOO<B> = 's' }
    s2();
    is %*FOO<B>, 's', 'a store from a sub into a user dynamic hash';
}

is (%*ENV<A>:exists), False, 'the process %*ENV is back after the block, without its key';
