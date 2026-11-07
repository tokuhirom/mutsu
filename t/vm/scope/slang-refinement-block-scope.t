use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang's action-role refinement ends with the block that holds the `use`
# (overload::constant's "only inside its scope", #15804).

plan 3;

sub integer { "i$^a" }

is 4, 4, 'before the use: untouched';
{
    use NumishHandlerSlang &integer;
    is 5, 'i5', 'inside the block the handler runs';
}
is 6, 6, 'after the block the literal is untouched again';
