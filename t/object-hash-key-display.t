use Test;

# Object hashes (`my %h{Type}`) store keys by `.WHICH` internally, but every
# place that exposes a key (pairs from .sort/.map/listification, gist, .raku,
# string context, .min/.max, slice adverbs) must show the *original* typed key,
# not the internal WHICH string (e.g. `1`, not `Int|1`).

plan 29;

# --- Int-keyed object hash ------------------------------------------------
my %i{Int};
%i{1} = "x"; %i{2} = "y"; %i{3} = "z";

is %i.sort.gist,          '(1 => x 2 => y 3 => z)', '.sort uses typed key';
is %i.gist,               '{1 => x, 2 => y, 3 => z}', '.gist uses typed key';
is %i.Str,                "1\tx\n2\ty\n3\tz",       '.Str uses typed key';
is %i.raku,               '(my Any %{Int} = 1 => "x", 2 => "y", 3 => "z")', '.raku uses typed key';
is %i.list.sort.gist,     '(1 => x 2 => y 3 => z)', '.list uses typed key';
is %i.pairs.sort.gist,    '(1 => x 2 => y 3 => z)', '.pairs uses typed key';
is %i.map({ "{.key}={.value}" }).sort.gist, '(1=x 2=y 3=z)', '.map sees typed key';
is %i.min.gist,           '1 => x',                 '.min returns typed-key pair';
is %i.max.gist,           '3 => z',                 '.max returns typed-key pair';
is %i.minpairs.gist,      '(1 => x)',               '.minpairs typed key';
is %i.maxpairs.gist,      '(3 => z)',               '.maxpairs typed key';

# keys / values themselves stay correct
is %i.keys.sort.gist,     '(1 2 3)',                '.keys typed';
is %i.values.sort.gist,   '(x y z)',                '.values';

# iteration yields the typed key
my @collected;
for %i.sort -> $p { @collected.push($p.key) }
is @collected.gist,       '[1 2 3]',                'for-iteration sees typed key';

# @-flattening preserves typed key in pairs
my @flat = %i;
is @flat.sort.gist,       '(1 => x 2 => y 3 => z)', '@-flatten uses typed key';

# --- slice adverbs on object hashes --------------------------------------
is (%i{1,2}).gist,        '(x y)',                  'plain slice';
is (%i{1,2}:exists).gist, '(True True)',            ':exists slice';
is (%i{1,2}:k).gist,      '(1 2)',                  ':k slice typed';
is (%i{1,2}:kv).gist,     '(1 x 2 y)',              ':kv slice typed';
is (%i{1,2}:p).gist,      '(1 => x 2 => y)',        ':p slice typed';
is (%i{2}:delete).gist,   'y',                      ':delete single';
is %i.keys.sort.gist,     '(1 3)',                  ':delete removed entry';

# --- Str-keyed object hash ------------------------------------------------
my %s{Str};
%s<alpha> = 1; %s<beta> = 2;
is %s.sort.gist,          '(alpha => 1 beta => 2)', 'Str object hash .sort';
is %s.raku,               '(my Any %{Str} = :alpha(1), :beta(2))', 'Str object hash .raku';
is %s.gist,               '{alpha => 1, beta => 2}', 'Str object hash .gist';

# --- plain hash is unaffected --------------------------------------------
my %p = a => 1, b => 2;
is %p.sort.gist,          '(a => 1 b => 2)',        'plain hash .sort';
is %p.raku,               '{:a(1), :b(2)}',         'plain hash .raku';
is %p.gist,               '{a => 1, b => 2}',       'plain hash .gist';
is %p.map({ .key }).sort.gist, '(a b)',             'plain hash .map';
