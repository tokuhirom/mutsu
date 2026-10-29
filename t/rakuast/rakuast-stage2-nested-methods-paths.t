use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing, with the names
# declared in the calling scope and with the names declared inside the
# evaluated unit. Family: a `method` declared in a nested block of a package
# body (a bare block, `do { }`, an `if` branch, a routine body) is the
# package's and closes over the enclosing block's lexicals.

my $outer = 41;

# A class is installed in its unit's package, so each use names its own.
my $serial = 0;
sub named(Str $code, Str $path) { $code.subst(:g, 'PATH', $path ~ ++$serial) }

# Declared inside the unit: the setup is part of the snippet.
my @inside =
    'do block',      'class CPATH { do { my sub h { 5 }; method m { h() } } }; CPATH.new.m',                '5',
    '.^methods',     'class CPATH { do { method hidden { 1 } } }; CPATH.^methods(:local).map(*.name).grep(/hidden/).join(",")', 'hidden',
    'has-scoped',    'class CPATH { do { my method pm { 9 }; method m { self.&pm } } }; CPATH.new.m',        '9';

# These close over a `my` variable of the nested block: the file path only.
# TODO: they die with "Variable '$n' is not declared" inside EVAL (#15468).
my @file-only =
    'bare block',    'class CPATH { { my $n = 7; method m { $n } } }; CPATH.new.m',                          '7',
    'if branch',     'class CPATH { if 1 { my $n = 8; method m { $n } } }; CPATH.new.m',                     '8',
    'two methods',   'class CPATH { do { my $n = 2; method a { $n }; method b { $n + 1 } } }; CPATH.new.a ~ CPATH.new.b', '23',
    'role',          'role RPATH { do { my $n = 3; method m { $n } } }; class XPATH does RPATH { }; XPATH.new.m', '3',
    'grammar',       'grammar GPATH { do { my $n = 4; method m { $n } }; token TOP { x } }; GPATH.new.m',   '4';

# Declared in the calling scope: the snippet only names them.
my @outside =
    'caller lexical', 'class CPATH { do { method m { $outer + 1 } } }; CPATH.new.m',                       '42';

sub as-file(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', "say do \{ $code }", :out, :err;
    $proc.out.slurp(:close).trim;
}

for @inside -> $name, $code, $expected {
    is as-file(named($code, 'F')), $expected, "$name: as a file";
    is EVAL(named($code, 'S')).Str, $expected, "$name: as an EVAL string";
    is EVAL(named($code, 'A').AST).Str, $expected, "$name: as EVAL of the string's .AST";
}

for @file-only -> $name, $code, $expected {
    is as-file(named($code, 'F')), $expected, "$name: as a file";
}

for @outside -> $name, $code, $expected {
    is EVAL(named($code, 'S')).Str, $expected, "$name (names in the caller): as an EVAL string";
    is EVAL(named($code, 'A').AST).Str, $expected, "$name (names in the caller): as EVAL of the .AST";
}

done-testing;
