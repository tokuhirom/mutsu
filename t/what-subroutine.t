use Test;

plan 5;

my $x = 42;
is (WHAT $x).gist, '(Int)', 'WHAT subroutine form works';
is WHAT($x).gist, '(Int)', 'WHAT subroutine call form works';

# A bare `{...}` right after WHAT is unambiguously its hash-literal/block
# argument, not a separate statement (todo/tickets/what-prefix-bare-hash-literal-block-arg.md).
is WHAT({3 => 4}).gist, '(Hash)', 'WHAT with parenthesized hash literal works';
is (WHAT {3 => 4}), Hash, 'WHAT with bare hash-literal block arg works';
is (WHAT { 1 + 1 }), Block, 'WHAT with bare non-hash block arg works';
