# A statement-leading hash literal `{...}` may carry postfix operators
# (`{a => 1}.keys`, `{a => 1}<b>`, `{a => 1}.map(...)`). Previously the hash
# terminated the statement and a following `.keys` was mis-parsed as a new
# `$_.keys` statement (so the value was lost / dispatched on the topic). This is
# the classic block-vs-hash disambiguation: a `{...}` term with a postfix is one
# expression, not a self-terminating block statement.
use Test;

plan 9;

# The block bodies below start with a hash literal carrying a postfix; the block
# is invoked so the body's value is observable.
is { {a => 1}.keys.join }(), 'a', 'hash literal . method (.keys.join)';
is { {a => 1, b => 2}.elems }(), 2, 'hash literal .elems';
is { {a => 1, b => 2}.map({ .value * 10 }).sort.join(',') }(), '10,20',
    'hash literal .map chain';
is { {a => 5}<a> }(), 5, 'hash literal <> subscript';
is { {a => 1, b => 2}.values.sort.join(',') }(), '1,2', 'hash literal .values';
is { {x => 10}.kv.join('=') }(), 'x=10', 'hash literal .kv';

# A bare hash literal statement (no postfix) is still a Hash, not a block.
is do { my $h = { {a => 1} }(); $h.^name }, 'Hash', 'bare hash literal stays a Hash';

# A bare block statement (not a hash) still runs as a block.
is do { my $n = 0; { $n = 7 }; $n }, 7, 'bare block statement still runs';

# Chained postfix on a hash literal at statement start.
is { {a => 1, b => 2, c => 3}.pairs.grep(*.value > 1).map(*.key).sort.join(',') }(),
    'b,c', 'hash literal .pairs.grep.map chain';
