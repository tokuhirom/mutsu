use v6;
use Test;
use JSON::Fast;

# to-json on user instances doing Associative/Positional serializes via .list
# (JSON::Fast t/12-assocpositional.t parity). Associative wins when both.

class Both does Positional does Associative {
    method list { List.new(|do Pair.new($_.Str, $_) for 3 ... 1) }
    method sort(|c) { self.list.sort(|c) }
    method of { self.Positional::of() }
}

my $expected = %( do $_.Str => $_ for 3 ... 1 );
for Bool::.values X Bool::.values -> ($pretty, $sorted-keys) {
    is-deeply from-json(to-json(Both.new, :$pretty, :$sorted-keys)), $expected,
        "roundtrip with :pretty($pretty) :sorted-keys($sorted-keys)";
}

class PosOnly does Positional {
    method list { (10, 20, 30) }
    method of { Mu }
}
is-deeply from-json(to-json(PosOnly.new, :!pretty)), $[10, 20, 30],
    'Positional-only instance serializes as an array';

# Nested inside plain data too.
is-deeply from-json(to-json([1, PosOnly.new], :!pretty)), $[1, $[10, 20, 30]],
    'instance nested in an array is converted';

done-testing;
