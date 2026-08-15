use Test;

# `.^can` / `.^methods` answered from a hardcoded per-type method list that had
# drifted from the methods mutsu actually dispatches: several real, working
# methods were missing, so `.^can` lied (returned False) and `.^methods` omitted
# them. These pin the methods back in sync — each one both works AND introspects.

plan 57;

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

# --- Int ---
works-and-can 5, 'expmod', { 4.expmod(2, 5) };

# --- List / Array ---
works-and-can (1, 2, 3), 'minpairs', { (1, 2, 3).minpairs };
works-and-can (1, 2, 3), 'maxpairs', { (1, 2, 3).maxpairs };

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
ok (1, 2).^methods.map(*.Str).grep('minpairs'),  'List.^methods includes minpairs';
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

# Methods that mutsu does NOT implement must still report False (no over-claim).
nok 'abc'.^can('samespace').Bool, 'unimplemented samespace is not over-claimed';
