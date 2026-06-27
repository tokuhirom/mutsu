use Test;

# `.^can` / `.^methods` answered from a hardcoded per-type method list that had
# drifted from the methods mutsu actually dispatches: several real, working
# methods were missing, so `.^can` lied (returned False) and `.^methods` omitted
# them. These pin the methods back in sync — each one both works AND introspects.

plan 20;

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

# The fixed names also appear in `.^methods`.
ok 'x'.^methods.map(*.Str).grep('trans'),        'Str.^methods includes trans';
ok 'x'.^methods.map(*.Str).grep('substr-rw'),    'Str.^methods includes substr-rw';
ok (1, 2).^methods.map(*.Str).grep('minpairs'),  'List.^methods includes minpairs';

# Methods that mutsu does NOT implement must still report False (no over-claim).
nok 'abc'.^can('samespace').Bool, 'unimplemented samespace is not over-claimed';
