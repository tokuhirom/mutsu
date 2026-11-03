use Test;
plan 6;

# `is cached` is experimental in Rakudo: refused at compile time unless the
# scope said `use experimental :cached` (#16273).

try EVAL 'sub f($x) is cached { $x }; 1';
isa-ok $!, X::Experimental, 'is cached without the pragma is X::Experimental';
like $!.message, /"'is cached'"/, 'the message names the trait';
is $!.use, 'cached', 'the exception names the pragma tag';

is EVAL('use experimental :cached; sub g($x) is cached { $x * 2 }; g(2)'), 4,
    'with the pragma the trait works';

is EVAL('use experimental :cached; { sub h($x) is cached { $x + 1 }; h(1) }'), 2,
    'a nested scope inherits the pragma';

{
    use experimental :cached;
    sub k($x) is cached { $x }
    is k(7), 7, 'the pragma covers the rest of its own scope';
}
