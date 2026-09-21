use Test;

plan 3;

# Found by PrettyDump 1.2.3: Match.hash is an immutable Map, while Match.Hash
# remains the mutable Hash coercion.
'abcdef' ~~ / cd /;
is $/.hash.^name, 'Map', 'Match.hash exposes a Map';
is $/.Hash.^name, 'Hash', 'Match.Hash exposes a Hash';
is %($/).^name, 'Map', '%() preserves the Match capture Map';
