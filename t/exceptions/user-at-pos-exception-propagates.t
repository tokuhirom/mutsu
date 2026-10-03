use Test;

# An exception thrown by a class's own AT-POS (`$obj[$i]` past its bounds)
# propagates to the caller. mutsu swallowed it into Nil, so `dies-ok` saw a
# living block and a CATCH around the read resumed with Nil. Reduced from
# the Rake distribution, whose AT-POS throws X::OutOfRange.

plan 4;

class P does Positional {
    method AT-POS($i) {
        $i < 2 ?? $i * 10 !! X::OutOfRange.new(what => 'index', got => $i, range => '0..1').throw
    }
}
my $p = P.new;
is $p[1], 10, 'in range';
dies-ok { $p[2] }, 'out of range dies';
throws-like { $p[5] }, X::OutOfRange, what => 'index', got => 5;
my @log;
try { @log.push($p[3]); CATCH { default { @log.push('caught') } } }
is-deeply @log, ['caught'], 'CATCH handles it and does not resume';
