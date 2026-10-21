use Test;

# MVM_COVERAGE_LOG names a file the interpreter appends `HIT  <file>  <line>`
# records to, one per statement line it runs (the protocol Code::Coverage speaks
# to MoarVM). Checked against `raku` first; rakudo also logs its own module
# loader's lines, so only the records of the script are compared.

plan 5;

my $dir    = $*TMPDIR.add("mvm-coverage-log-$*PID-{now.Num.Int}");
$dir.mkdir;
LEAVE { .unlink for $dir.dir; $dir.rmdir }
my $script = $dir.add('prog.raku');
my $log    = $dir.add('hits.log');
$script.spurt(q:to/CODE/);
    my $x = 1;
    if $x { say "yes" } else { say "no" }
    sub f($n) {
        $n + 1
    }
    say f(2);
    CODE

sub lines-hit(IO::Path $file, IO::Path $log --> List) {
    my $needle = "HIT  {$file.absolute}  ";
    $log.lines.grep(*.starts-with($needle)).map(*.substr($needle.chars).Int).sort.squish.List
}

temp %*ENV;
%*ENV<MVM_COVERAGE_LOG> = $log.absolute;
my $proc = run $*EXECUTABLE, $script.absolute, :out, :err;
is $proc.exitcode, 0, 'the program under measurement runs normally';
is $proc.out.slurp, "yes\n3\n", 'and prints what it always prints';
ok $log.e, 'the log file was written';
is-deeply lines-hit($script, $log), (1, 2, 3, 4, 6),
    'every statement line that ran was hit, the untaken else branch is on line 2';

%*ENV<MVM_COVERAGE_LOG>:delete;
my $plain = run $*EXECUTABLE, $script.absolute, :out, :err;
is $plain.out.slurp, "yes\n3\n", 'without the variable the program runs as before';
