use Test;

plan 7;

# Nested Pair sub-signature destructuring must not drop the inner value when
# the destructured value is itself a string-key `Pair`. A named sub-param's
# candidate already comes from the source pair's `.value`, so it must not be
# unwrapped again. Previously a chained `a => "b" => c` (whose inner pair is a
# string-key Pair) bound the innermost value to the outer param, throwing
# "expected Pair, got ...". The parenthesised form (a ValuePair inner) already
# worked; this aligns the string-key form with it.

# Two-level destructuring, chained listop call (the form roast / Test::Util use).
{
    sub f(Pair (Int:D :key($plan), Pair :value((Str:D :key($desc), :value($val))))) {
        "$plan|$desc|$val"
    }
    is f(2 => "g" => 5),   '2|g|5', 'chained pair, paren call';
    is (f 2 => "g" => 5),  '2|g|5', 'chained pair, listop call';
}

# The inner value may itself be a block/callable (the Test::Util `group-of` shape).
{
    my $ran = 0;
    sub run-group(Pair (Int:D :key($n), Pair :value((Str:D :key($d), :value(&code))))) {
        "$n|$d|" ~ code()
    }
    is (run-group 3 => "desc" => { 42 }), '3|desc|42', 'inner block value runs';
}

# A non-nested `:value($v)` whose value is a Pair keeps the whole Pair.
{
    sub h(Pair (:key($k), :value($v))) { "$k|" ~ $v.^name ~ "|" ~ $v.key ~ "|" ~ $v.value }
    is (h 1 => "a" => 2), '1|Pair|a|2', 'listop: value stays a Pair';
    is h(1 => ("a" => 2)), '1|Pair|a|2', 'paren: value stays a Pair';
}

# Plain (non-pair) inner values still bind correctly.
{
    sub p(Pair (:key($k), Pair :value((:key($k2), :value($v2))))) { "$k|$k2|$v2" }
    is (p 2 => 3 => 5), '2|3|5', 'all-Int chained pair';
    is p(2 => (3 => 5)), '2|3|5', 'all-Int paren pair';
}
