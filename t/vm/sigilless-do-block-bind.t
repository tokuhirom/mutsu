use Test;

plan 8;

# A sigilless declaration with a `do` initializer binds the container of the
# block's tail, so a Proxy stays a Proxy (#16965).
my @log;

my \paren = (Proxy.new(FETCH => -> $ { 1 }, STORE => -> $, $v { @log.push("paren $v") }));
paren = 5;
is @log.join(','), 'paren 5', 'parenthesized source keeps the Proxy';
is paren, 1, 'FETCH still runs on read';

my \dostmt = do Proxy.new(FETCH => -> $ { 1 }, STORE => -> $, $v { @log.push("stmt $v") });
dostmt = 4;
is @log.join(','), 'paren 5,stmt 4', 'do EXPR source keeps the Proxy';
is dostmt, 1, 'FETCH runs on read (do EXPR)';

my \doblock = do { Proxy.new(FETCH => -> $ { 1 }, STORE => -> $, $v { @log.push("block $v") }) };
doblock = 3;
is @log.join(','), 'paren 5,stmt 4,block 3', 'do { EXPR } source keeps the Proxy';

my \many = do { my $x = 1; Proxy.new(FETCH => -> $ { 1 }, STORE => -> $, $v { @log.push("many $v") }) };
many = 2;
is @log.elems, 4, 'do block with leading statements keeps the Proxy';

# Plain values are unaffected.
my \plain = do { 42 };
is plain, 42, 'a do block value still binds';
dies-ok { EVAL 'my \q = do { 7 }; q = 8' }, 'a plain value stays immutable';
