use Test;

plan 16;

# `{ ... }` composes a hash when the *first element of the first statement* is a
# fatarrow pair, and is a block otherwise. Deciding that from a syntactic prefix
# table missed every computed key, so `constant %m = { +(A) => Str }` — the shape
# DBDish::Oracle::Native uses — parsed as a block and died with
# "Odd number of elements found where hash initializer expected".

constant A = 1;
constant B = 2;

# Computed keys.
is-deeply { +(A) => 'x', +(B) => 'y' }, {'1' => 'x', '2' => 'y'},
    'a prefix-op key composes a hash';
is-deeply { 'a' ~ 'b' => 1 }, {ab => 1}, 'a concatenation key composes a hash';
is-deeply { (1) => 'p' }, {'1' => 'p'}, 'a parenthesised key composes a hash';
constant %m = { +(A) => Str, +(B) => Int };
is %m.keys.sort.join(','), '1,2', 'a constant hash with computed keys';

# Still hashes.
isa-ok { a => 1 }, Hash, 'a bareword key is still a hash';
isa-ok { a => 1; }, Hash, 'a trailing semicolon does not make it a block';
isa-ok { 1 R=> 2 }, Hash, 'a reverse fatarrow composes a hash';
isa-ok { }, Hash, 'an empty brace pair is still a hash';

# Blocks: the fatarrow is not the first element's outermost operator.
isa-ok { 1, 2 => 3 }, Block, 'a non-pair first element makes it a block';
isa-ok { (1 => 2) }, Block, 'a parenthesised pair is not a top-level pair';
isa-ok { 1 <=> 2 }, Block, '<=> is not a fatarrow';
isa-ok { my %h = a => 1; %h }, Block, 'an assignment of a pair is a block';
isa-ok { $_ => 1 }, Block, 'an explicit topic makes it a block';

# An invocant-less method call is a topic reference too.
isa-ok { .key => 1 }, Block, 'a leading-dot key makes it a block';
isa-ok { a => .key }, Block, 'a leading-dot value makes it a block';
# ... but one inside an interpolation belongs to that interpolation's closure.
isa-ok { "{ .^name }X" => 1 }, Hash, 'a dot call inside a string does not count';
