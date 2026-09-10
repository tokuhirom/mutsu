use Test;

plan 8;

# A pointy block may take more than one parameter *and* destructure them:
# `-> [$a, $b], [$c, $d]` binds one chunk element per pattern and unpacks each.
# Only a single pattern used to parse; a second one left the `{` unconsumed and
# the loop reported X::Syntax::Missing.

{
    my @seen;
    for (1, 2), (3, 4) -> [$a, $b], [$c, $d] {
        @seen.push: "$a$b$c$d";
    }
    is @seen, ['1234'], 'two bracket patterns unpack their own element';
}

{
    my @seen;
    for (1, 2), (3, 4) -> ($a, $b), ($c, $d) {
        @seen.push: "$a$b$c$d";
    }
    is @seen, ['1234'], 'two paren patterns unpack their own element';
}

{
    my @seen;
    for (1, 2), 3 -> [$a, $b], $c {
        @seen.push: "$a$b$c";
    }
    is @seen, ['123'], 'a pattern mixes with a plain parameter';
}

{
    my @seen;
    for 1, (2, 3) -> $a, [$b, $c] {
        @seen.push: "$a$b$c";
    }
    is @seen, ['123'], 'a plain parameter mixes with a pattern';
}

# More than one iteration: the chunk advances two elements at a time.
{
    my @seen;
    for (1, 2), (3, 4), (5, 6), (7, 8) -> [$a, $b], [$c, $d] {
        @seen.push: "$a$b-$c$d";
    }
    is @seen, ['12-34', '56-78'], 'the loop batches by the parameter count';
}

# The shape the Cro router test uses: an Array-keyed hash walked with .kv.
{
    my %h{Array} = ['/outer', 1] => ['a', 'first'];
    my @seen;
    for %h.kv -> [$target, $variant], [$body, $desc] {
        @seen.push: "$target $variant $body $desc";
    }
    is @seen, ['/outer 1 a first'], 'Array-keyed hash .kv destructures both sides';
}

# A single pattern still destructures the whole iteration value.
{
    my @seen;
    for ((1, 2), (3, 4)) -> [$a, $b] {
        @seen.push: "$a$b";
    }
    is @seen, ['12', '34'], 'one pattern still unpacks the whole element';
}

# Named destructuring works per pattern too.
{
    my @seen;
    for %(:a(1)), %(:b(2)) -> (:$a), (:$b) {
        @seen.push: "{$a // 'x'}{$b // 'y'}";
    }
    is @seen, ['12'], 'named patterns unpack their own element';
}
