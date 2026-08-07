use Test;

# `BEGIN` runs at compile time but is still an ordinary value-producing
# statement: in value-final position it is the block's value. Cro's
# `Cro::HTTP::Body::MultiPartFormData::Part` uses it for a default --
# `else { BEGIN Cro::MediaType.new(type => 'text', subtype-name => 'plain') }` --
# so a part with no Content-Type header had no content type at all.

plan 10;

# Already worked: BEGIN as an expression operand.
{
    my $a = BEGIN 42;
    is $a, 42, 'BEGIN as a declaration initialiser';
    is (BEGIN 1 + 2), 3, 'BEGIN inline in an expression';
    my $b = BEGIN { 7 * 6 };
    is $b, 42, 'BEGIN block as a declaration initialiser';
}

# The gap: BEGIN as the final *statement* of a block.
{
    sub tail() { BEGIN 'hello' }
    is tail(), 'hello', 'BEGIN in routine tail position is the return value';

    sub fallback($h) { with $h { "got $_" } else { BEGIN 'default' } }
    is fallback(Nil), 'default', 'BEGIN at the tail of an else branch is its value';
    is fallback(1), 'got 1', 'and the taken branch is unaffected';

    sub in-given($x) {
        given $x {
            when Int { BEGIN 'int' }
            default  { BEGIN 'other' }
        }
    }
    is in-given(1), 'int', 'BEGIN at the tail of a when block is its value';
    is in-given('s'), 'other', 'BEGIN at the tail of a default block is its value';
}

# BEGIN evaluates exactly once, even in a routine called repeatedly.
{
    my $runs = 0;
    sub counted() { BEGIN { $runs++; 'once' } }
    counted() for ^3;
    is counted(), 'once', 'the memoized BEGIN keeps answering its value';
    # raku runs the body at true compile time (so the runtime `$runs` is still
    # 0); mutsu memoizes it at first use (1). Either way it must not re-run per
    # call, which is what regressed.
    ok $runs <= 1, 'the BEGIN body does not re-run on every call';
}
