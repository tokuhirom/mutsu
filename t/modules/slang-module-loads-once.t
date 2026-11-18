use Test;

# A slang module loads once per process, when the parser reaches its `use`
# (ADR-17558 slice 4), as on rakudo: the interpreter that runs the program
# loads it at parse time with `$*LANG` bound, so its `EXPORT` hook registers
# the slang for the parse, and the `use` in position only imports. It used to
# load a second time in a separate activation interpreter at parse time.

plan 3;

my $log = $*TMPDIR.add("mutsu-counted-slang-$*PID.log");
$log.unlink;
my $program = q:to/END/;
    use lib 't/lib';
    say 'before';
    use CountedSlang;
    say 'after';
    END
my $proc = run $*EXECUTABLE, '-I', 'modules/Slangify/lib', '-e', $program, :out, :err, :env(%*ENV, COUNTED_SLANG_LOG => ~$log);
is $proc.out.slurp(:close), "before\nafter\n", 'the program runs';
is $proc.exitcode, 0, 'and exits cleanly';
is ($log.e ?? $log.slurp !! ''), "mainline\n", "the slang module's mainline ran once";
$log.unlink;
