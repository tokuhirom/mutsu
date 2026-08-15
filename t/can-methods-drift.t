use Test;

# `.^can` / `.^methods` answered from a hardcoded per-type method list that had
# drifted from the methods mutsu actually dispatches: several real, working
# methods were missing, so `.^can` lied (returned False) and `.^methods` omitted
# them. These pin the methods back in sync — each one both works AND introspects.

plan 193;

# Helper: the method genuinely dispatches (so the list entry is honest) and
# `.^can` agrees.
sub works-and-can($obj, Str $name, &call) {
    lives-ok &call, "$name dispatches";
    ok $obj.^can($name).Bool, "$name is reported by .^can";
}

# --- Str ---
works-and-can 'abc', 'trans',        { 'abc'.trans('a' => 'x') };
works-and-can 'abc', 'subst',        { 'abc'.subst('a', 'X') };
works-and-can 'abc', 'subst-mutate', { my $s = 'abc'; $s.subst-mutate(/a/, 'X') };
works-and-can 'abc', 'substr-rw',    { my $s = 'abc'; $s.substr-rw(0, 1) = 'Z' };
works-and-can 'abc', 'substr-eq',    { 'abc'.substr-eq('bc', 1) };
works-and-can 'A', 'uniprop',   { 'A'.uniprop('Alpha') };
works-and-can 'hi', 'indent',   { 'hi'.indent(2) };
works-and-can 'A', 'ord',       { 'A'.ord };
works-and-can 'A', 'uniname',   { 'A'.uniname };
works-and-can 'AB', 'uninames', { 'AB'.uninames };
works-and-can '5', 'unival',    { '5'.unival };
works-and-can '12', 'univals',  { '12'.univals };
works-and-can 'hi', 'tclc',     { 'hi'.tclc };
works-and-can '1.2.3', 'Version',  { '1.2.3'.Version };
works-and-can '2024-01-01', 'Date', { '2024-01-01'.Date };
works-and-can '2024-01-01T00:00:00Z', 'DateTime', { '2024-01-01T00:00:00Z'.DateTime };

# --- Int ---
works-and-can 5, 'expmod',  { 4.expmod(2, 5) };
works-and-can 5, 'rand',    { 5.rand };
works-and-can 65, 'uniprop', { 65.uniprop('Alpha') };
works-and-can 5, 'lsb',     { 5.lsb };
works-and-can 5, 'msb',     { 5.msb };
works-and-can 5, 'Real',    { 5.Real };

# --- Rat ---
works-and-can (1/3), 'FatRat', { (1/3).FatRat };
works-and-can (1/3), 'nude',   { (1/3).nude };

# --- Complex ---
works-and-can (1+2i), 'isNaN', { (1+2i).isNaN };
works-and-can (1+2i), 're',    { (1+2i).re };
works-and-can (1+2i), 'im',    { (1+2i).im };
works-and-can (1+2i), 'reals', { (1+2i).reals };
works-and-can (1+2i), 'conj',  { (1+2i).conj };
works-and-can (1+2i), 'Complex', { (1+2i).Complex };

# --- List / Array ---
works-and-can (1, 2, 3), 'minpairs', { (1, 2, 3).minpairs };
works-and-can (1, 2, 3), 'maxpairs', { (1, 2, 3).maxpairs };
works-and-can (1, 2, 3), 'list',        { (1, 2, 3).list };
works-and-can (1, 2, 3), 'item',        { (1, 2, 3).item };
works-and-can (1, 2, 3), 'Slip',        { (1, 2, 3).Slip };
works-and-can (1, 2, 3), 'sink',        { (1, 2, 3).sink };
works-and-can (a => 1, b => 2), 'invert', { (a => 1, b => 2).invert };
works-and-can (1, 2, 3), 'AT-POS',      { (1, 2, 3).AT-POS(0) };
works-and-can (1, 2, 3), 'EXISTS-POS',  { (1, 2, 3).EXISTS-POS(0) };
works-and-can (1, 2, 3), 'is-lazy',     { (1, 2, 3).is-lazy };
works-and-can (1, 2, 3), 'Capture',     { (1, 2, 3).Capture };
works-and-can (1, 2, 3), 'hyper',       { (1, 2, 3).hyper };
works-and-can (1, 2, 3), 'race',        { (1, 2, 3).race };
works-and-can (1, 2, 3), 'Supply',      { (1, 2, 3).Supply };
works-and-can (1, 2, 3), 'fmt',         { (1, 2, 3).fmt('%d') };

my @arr = (1, 2, 3);
works-and-can @arr, 'WHICH',   { @arr.WHICH };
works-and-can @arr, 'dynamic', { @arr.dynamic };

# --- Range ---
my $rng = 1..5;
works-and-can $rng, 'hyper',       { $rng.hyper };
works-and-can $rng, 'lazy',        { $rng.lazy };
works-and-can $rng, 'int-bounds',  { $rng.int-bounds };
works-and-can $rng, 'AT-POS',      { $rng.AT-POS(0) };
works-and-can $rng, 'race',        { $rng.race };
works-and-can $rng, 'in-range',    { $rng.in-range(3) };
works-and-can $rng, 'EXISTS-POS',  { $rng.EXISTS-POS(0) };

# --- Blob / Buf ---
my $buf = Buf.new(1, 2, 3, 4, 5, 6, 7, 8);
works-and-can $buf, 'read-uint8',  { $buf.read-uint8(0) };
works-and-can $buf, 'read-int8',   { $buf.read-int8(0) };
works-and-can $buf, 'read-uint16', { $buf.read-uint16(0) };
works-and-can $buf, 'read-int16',  { $buf.read-int16(0) };
works-and-can $buf, 'read-uint32', { $buf.read-uint32(0) };

# --- Mu ---
works-and-can 5, 'DEFINITE', { 5.DEFINITE };

# --- Any ---
works-and-can (1, 2, 3), 'serial', { (1, 2, 3).serial };
works-and-can (a => 1, b => 2), 'hash', { (a => 1, b => 2).hash };

# --- Hash ---
my %h = a => 1, b => 2;
works-and-can %h, 'pick',        { %h.pick };
works-and-can %h, 'EXISTS-KEY',  { %h.EXISTS-KEY('a') };
works-and-can %h, 'AT-KEY',      { %h.AT-KEY('a') };
works-and-can %h, 'List',        { %h.List };
works-and-can %h, 'invert',      { %h.invert };
works-and-can %h, 'flat',        { %h.flat };
works-and-can %h, 'dynamic',     { %h.dynamic };
works-and-can %h, 'roll',        { %h.roll };

# --- Cool (native-sized-integer coercion methods) ---
works-and-can 300, 'int8', { 300.int8 };

# A type that is NOT Cool-derived must not spuriously pick up Cool's own
# coercion methods -- regression pin for the `is_builtin_type_method`
# ancestor-list bug this Cool-list growth exposed (it used to hardcode
# ["Cool", "Any", "Mu"] as ancestors for every type, unconditionally).
nok (a => 1).^can('int8').Bool, 'Pair is not Cool, so it cannot int8';

# The fixed names also appear in `.^methods`.
ok 'x'.^methods.map(*.Str).grep('trans'),        'Str.^methods includes trans';
ok 'x'.^methods.map(*.Str).grep('substr-rw'),    'Str.^methods includes substr-rw';
ok 'x'.^methods.map(*.Str).grep('uniprop'),      'Str.^methods includes uniprop';
ok 'x'.^methods.map(*.Str).grep('indent'),       'Str.^methods includes indent';
ok 'x'.^methods.map(*.Str).grep('ord'),          'Str.^methods includes ord';
ok 'x'.^methods.map(*.Str).grep('uniname'),      'Str.^methods includes uniname';
ok 'x'.^methods.map(*.Str).grep('uninames'),     'Str.^methods includes uninames';
ok 'x'.^methods.map(*.Str).grep('unival'),       'Str.^methods includes unival';
ok 'x'.^methods.map(*.Str).grep('univals'),      'Str.^methods includes univals';
ok 'x'.^methods.map(*.Str).grep('tclc'),         'Str.^methods includes tclc';
ok 'x'.^methods.map(*.Str).grep('Version'),      'Str.^methods includes Version';
ok 'x'.^methods.map(*.Str).grep('Date'),         'Str.^methods includes Date';
ok 'x'.^methods.map(*.Str).grep('DateTime'),     'Str.^methods includes DateTime';
ok (1, 2).^methods.map(*.Str).grep('minpairs'),  'List.^methods includes minpairs';
ok (1, 2).^methods.map(*.Str).grep('Slip'),      'List.^methods includes Slip';
ok (1, 2).^methods.map(*.Str).grep('hyper'),     'List.^methods includes hyper';
ok (1, 2).^methods.map(*.Str).grep('fmt'),       'List.^methods includes fmt';
ok @arr.^methods.map(*.Str).grep('WHICH'),       'Array.^methods includes WHICH';
ok @arr.^methods.map(*.Str).grep('dynamic'),     'Array.^methods includes dynamic';
ok $rng.^methods.map(*.Str).grep('hyper'),       'Range.^methods includes hyper';
ok $rng.^methods.map(*.Str).grep('int-bounds'),  'Range.^methods includes int-bounds';
ok $rng.^methods.map(*.Str).grep('in-range'),    'Range.^methods includes in-range';
ok $buf.^methods.map(*.Str).grep('read-uint8'),  'Blob.^methods includes read-uint8';
ok $buf.^methods.map(*.Str).grep('read-uint32'), 'Blob.^methods includes read-uint32';
ok Mu.^methods.map(*.Str).grep('DEFINITE'),      'Mu.^methods includes DEFINITE';
ok Any.^methods.map(*.Str).grep('serial'),       'Any.^methods includes serial';
ok Any.^methods.map(*.Str).grep('hash'),         'Any.^methods includes hash';
ok %h.^methods.map(*.Str).grep('pick'),          'Hash.^methods includes pick';
ok %h.^methods.map(*.Str).grep('EXISTS-KEY'),    'Hash.^methods includes EXISTS-KEY';
ok %h.^methods.map(*.Str).grep('AT-KEY'),        'Hash.^methods includes AT-KEY';
ok %h.^methods.map(*.Str).grep('List'),          'Hash.^methods includes List';
ok %h.^methods.map(*.Str).grep('invert'),        'Hash.^methods includes invert';
ok %h.^methods.map(*.Str).grep('flat'),          'Hash.^methods includes flat';
ok %h.^methods.map(*.Str).grep('dynamic'),       'Hash.^methods includes dynamic';
ok %h.^methods.map(*.Str).grep('roll'),          'Hash.^methods includes roll';
ok Cool.^methods.map(*.Str).grep('int8'),        'Cool.^methods includes int8';
ok 5.^methods.map(*.Str).grep('rand'),           'Int.^methods includes rand';
ok 5.^methods.map(*.Str).grep('uniprop'),        'Int.^methods includes uniprop';
ok 5.^methods.map(*.Str).grep('lsb'),            'Int.^methods includes lsb';
ok 5.^methods.map(*.Str).grep('msb'),            'Int.^methods includes msb';
ok 5.^methods.map(*.Str).grep('Real'),           'Int.^methods includes Real';
ok 5.^methods.map(*.Str).grep('int8'),           'Int.^methods includes int8';
ok (1/3).^methods.map(*.Str).grep('FatRat'),     'Rat.^methods includes FatRat';
ok (1/3).^methods.map(*.Str).grep('nude'),       'Rat.^methods includes nude';
ok (1+2i).^methods.map(*.Str).grep('isNaN'),     'Complex.^methods includes isNaN';
ok (1+2i).^methods.map(*.Str).grep('re'),        'Complex.^methods includes re';
ok (1+2i).^methods.map(*.Str).grep('im'),        'Complex.^methods includes im';
ok (1+2i).^methods.map(*.Str).grep('reals'),     'Complex.^methods includes reals';
ok (1+2i).^methods.map(*.Str).grep('conj'),      'Complex.^methods includes conj';

# Methods that mutsu does NOT implement must still report False (no over-claim).
nok 'abc'.^can('samespace').Bool, 'unimplemented samespace is not over-claimed';
