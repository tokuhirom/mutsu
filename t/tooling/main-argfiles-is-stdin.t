use Test;

plan 3;

# From 6.d on, `$*ARGFILES` is `$*IN` inside `sub MAIN` (roast MISC/misc.t,
# #16412): the mainline's `lines` reads the files named on the command line,
# and the same call inside MAIN reads standard input.
my $dir = $*TMPDIR.add("mutsu-argfiles-{$*PID}-{(^1000000).pick}");
$dir.mkdir;
LEAVE { .unlink for $dir.dir; $dir.rmdir }
my @files = <THE FILES CONTENT>.map: -> $c { my $f = $dir.add($c.lc); $f.spurt("$c\n"); $f.Str };

sub run-script(Str $source, Str $stdin) {
    my $script = $dir.add('script.raku');
    $script.spurt($source);
    my $proc = run $*EXECUTABLE, $script.Str, |@files, :in, :out, :err;
    $proc.in.print($stdin);
    $proc.in.close;
    $proc.out.slurp(:close)
}

my $expected = "THE\nFILES\nCONTENT\nblah\nbleh\nbloh\n";
is run-script("use v6.d;\n.say for lines;\nsub MAIN(*@args) \{\n    .say for lines;\n\}\n", "blah\nbleh\nbloh"),
    $expected, 'MAIN is an only sub';
is run-script("use v6.d;\n.say for lines;\nmulti MAIN(\$, \$, \$) \{\n    .say for lines;\n\}\nmulti MAIN(|) \{ die \}\n", "blah\nbleh\nbloh"),
    $expected, 'MAIN is a multi sub';
is run-script("use v6.d;\n.say for lines;\n", "blah\nbleh\nbloh"),
    "THE\nFILES\nCONTENT\n", 'no MAIN: the mainline reads the files only';
