use Test;

# `-c` stops before the mainline but the unit's types are declared, as in
# rakudo, so CHECK-time code (static::CLI's analysis) can look their methods up.

plan 4;

sub compile(Str $code) {
    my $proc = run $*EXECUTABLE, '-c', '-e', $code, :out, :err;
    $proc.exitcode => ($proc.out.slurp(:close) ~ $proc.err.slurp(:close)).chomp
}

my $check = 'CHECK { note "m: ", C.^find_method("m").defined }';
my $r = compile('class C { method m() { 1 } }; ' ~ $check ~ '; note "RAN"');
is $r.key, 0, '-c succeeds';
like $r.value, /'m: True'/, 'a class is declared when the CHECK phaser runs';
unlike $r.value, /RAN/, 'the mainline does not run';
$r = compile('role R { method m() { 1 } }; class C does R { }; ' ~ $check);
like $r.value, /'m: True'/, 'a role composed into a class is too';
