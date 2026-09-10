use Test;

plan 22;

# A Num renders in scientific notation once its magnitude leaves the window
# [1e-4, 1e15) — both in string context (`.Str`/`.gist`/`~`) and in `.raku`,
# with the exponent written with an explicit sign and a 2-digit minimum
# (`1e-05`, `1e+20`, `1e-100`). (A Rat like `0.00001` always stays decimal.)

# --- string context (.Str / say) ---
is ~1e-5,   '1e-05',  'small Num stringifies in scientific notation';
is ~1.5e-5, '1.5e-05', 'small fractional Num scientific';
is ~9.99e-5,'9.99e-05', 'just under 1e-4 is scientific';
is ~1e-4,   '0.0001', '1e-4 exactly stays decimal';
is ~1e-10,  '1e-10',  'tiny Num scientific with 2-digit exponent';
is ~5e-8,   '5e-08',  'exponent zero-padded to 2 digits';
is ~1e15,   '1e+15',  'large Num scientific at 1e15';
is ~9e14,   '900000000000000', 'just under 1e15 stays decimal';
is ~1.5,    '1.5',    'normal-range Num stays decimal';
is ~0.001,  '0.001',  'normal small Num stays decimal';

# --- .raku ---
is (1e-5).raku,    '1e-05',   '.raku small Num scientific';
is (1.5e-5).raku,  '1.5e-05', '.raku small fractional scientific';
is (1e15).raku,    '1e+15',   '.raku large Num scientific';
is (1e14).raku,    '100000000000000e0', '.raku just under 1e15 is fixed with e0';
is (1e20).raku,    '1e+20',   '.raku positive exponent has + sign';
is (1e-100).raku,  '1e-100',  '.raku 3-digit exponent unpadded';
is (1.23456e-6).raku, '1.23456e-06', '.raku keeps mantissa, pads exponent';
is (-1e-5).raku,   '-1e-05',  '.raku negative small Num';
is (0.1).raku,     '0.1',     '.raku normal Num stays decimal';

# --- a Rat literal is unaffected ---
is ~0.00001, '0.00001', 'a Rat stays decimal even below 1e-4';
is (0.00001).WHAT.^name, 'Rat', '0.00001 is a Rat, not a Num';

# --- inside a list .raku ---
is [1e-5, 1e15, 2.5].raku, '[1e-05, 1e+15, 2.5]', 'scientific rendering inside a list';
