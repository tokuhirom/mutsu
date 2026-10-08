use Test;

plan 18;

# Version rows (ADR-11276): Str, gist, raku, WHICH, Version and ACCEPTS.
my $v = v1.2.3+;
is $v.Str, '1.2.3+', 'Version.Str has no v prefix';
is $v.gist, 'v1.2.3+', 'Version.gist is the v literal';
is $v.raku, 'v1.2.3+', 'Version.raku is the v literal';
is $v.WHICH, 'Version|1.2.3+', 'Version.WHICH is the canonical text';
is $v.Version.raku, 'v1.2.3+', 'Version.Version is itself';
is Version.new('1.2').raku, 'v1.2', 'Version.new(...).raku';
is (v1.2.3-).Str, '1.2.3-', 'minus suffix';

ok (v1.2.*).ACCEPTS(v1.2.5), 'wildcard matcher accepts a match';
nok (v1.2.*).ACCEPTS(v1.3), 'wildcard matcher rejects a mismatch';
ok (v1.2.3).ACCEPTS(v1.2.3), 'equal versions accept';
nok (v1.2.3).ACCEPTS(v1.2.4), 'different versions do not';
ok (v1.2+).ACCEPTS(v1.5), 'plus matcher accepts a later version';
ok v1.2.3 ~~ v1.2.*, 'smartmatch against a wildcard version';

# Bool rows: Int, Numeric and Real of an enum value.
is True.Int, 1, 'True.Int';
is False.Numeric, 0, 'False.Numeric';
is True.Real, 1, 'True.Real';
isa-ok True.Int, Int, 'Bool.Int is an Int';
is False.Int.raku, '0', 'False.Int.raku';

done-testing;
