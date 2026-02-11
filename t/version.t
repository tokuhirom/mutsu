use Test;
plan 19;

# Basic version literal
is v1.2.3, '1.2.3', 'version literal stringifies';
is v1.2.3.WHAT, '(Version)', 'version literal is Version type';

# Version with + and -
is v1.2+, '1.2+', 'version literal with +';
is v1.2.3-, '1.2.3-', 'version literal with -';

# Version with * wildcard
is v1.*.3, '1.*.3', 'version literal with *';

# eqv
ok v1.2 eqv v1.2, 'eqv same';
nok v1.2 eqv v1.3, 'eqv different';
ok v1.2 eqv v1.2.0, 'eqv trailing zero';

# ===
ok v1.2 === v1.2, '=== same';
nok v1.2 === v1.3, '=== different';

# Smart match
ok v1.2 ~~ v1.2, 'smartmatch same';
nok v1.2 ~~ v9.2, 'smartmatch different';
ok v1.2.3 ~~ v1, 'smartmatch prefix';
ok v1.2 ~~ v1.0+, 'smartmatch plus';
nok v1.2 ~~ v1.3+, 'smartmatch plus fails';

# cmp
is (v1.2 cmp v1.2), Same, 'cmp Same';
is (v1.2 cmp v3.2), Less, 'cmp Less';
is (v1.2 cmp v0.2), More, 'cmp More';

# Version.new
is Version.new("1.2.3").gist, 'v1.2.3', 'Version.new from string';
