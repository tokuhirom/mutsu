use Test;

plan 22;

# A Version keeps the *source spelling* of its parts for stringification, the
# way Rakudo does: `v1.02.3` stringifies as "1.02.3" while `.parts` still
# reports the parsed Ints `(1, 2, 3)`. Only the separators are normalized to
# `.`, and a leading/trailing `+`/`-` marker stays a flag.

is (v1.02.3).Str, '1.02.3', 'Str keeps a zero-padded part';
is ~(v1.02.3), '1.02.3', 'string context keeps a zero-padded part';
is (v1.02.3).gist, 'v1.02.3', 'gist keeps a zero-padded part';
is (v1.02.3).raku, 'v1.02.3', 'raku keeps a zero-padded part';
is (v01.2).Str, '01.2', 'a zero-padded leading part is kept';
is Version.new('1.02.3').Str, '1.02.3', 'Version.new keeps the spelling';
is Version.new('1.02.3').raku, 'v1.02.3', 'Version.new .raku keeps the spelling';
is '1.02.3'.Version.Str, '1.02.3', '.Version coercion keeps the spelling';
is Version.new(v1.02.3).Str, '1.02.3', 'Version.new(Version) keeps the spelling';

# ... but the parts themselves are still parsed numbers.
is-deeply (v1.02.3).parts.List, (1, 2, 3), 'parts are the parsed Ints';

# Ordinary versions are untouched.
is (v1.2.3).Str, '1.2.3', 'a plain version is unchanged';
is (v6.d.PREVIEW).Str, '6.d.PREVIEW', 'alphabetic parts are unchanged';
is (v6.0.0+).Str, '6.0.0+', 'the `+` marker still stringifies';
my $minus = Version.new('1.2.3-');
is $minus.Str, '1.2.3-', 'the `-` marker still stringifies';

# Separators are normalized to `.`, matching Rakudo.
is Version.new('1.2-beta').Str, '1.2.beta', 'a `-` separator normalizes to `.`';
is Version.new('1.2/3').Str, '1.2.3', 'a `/` separator normalizes to `.`';

# Comparison ignores the spelling: a zero-padded part is numerically equal.
ok v1.02.3 == v1.2.3, 'a zero-padded version is == its plain spelling';
is (v1.02.3 cmp v1.2.3), Same, 'cmp sees them as the same';
ok (v1.02.3 eqv v1.2.3), 'eqv sees them as the same';

# ... but identity (WHICH) is the canonical string, so they are not `===`.
is (v1.02.3).WHICH.Str, 'Version|1.02.3', 'WHICH is Version|<canonical string>';
nok (v1.02.3 === v1.2.3), 'differently spelled versions are not ===';
ok (v1.2 === v1.2), 'identically spelled versions are ===';

# vim: expandtab shiftwidth=4
