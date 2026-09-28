use Test;

plan 7;

# `Rakudo::Internals.INCLUDE` is the running process's `-I` list, as a List
# of Str. RakuDoc::Test::Files' t/01-methods.rakutest forwards it to a child
# `$*EXECUTABLE` so the child can `use` the distribution under test:
#
#     for Rakudo::Internals.INCLUDE { @args.push: '-I'; @args.push: $_ }
#
# which died with "No such method 'INCLUDE' for invocant of type
# 'Rakudo::Internals'".

my $exe = $*EXECUTABLE;
my $lib = 't/fixtures/lib-precedence/plain';

sub run-code(*@args, :%env = %*ENV) {
    my $r = run($exe, |@args, :out, :err, :%env);
    my $out = $r.out.slurp(:close).trim;
    my $err = $r.err.slurp(:close).trim;
    $r.exitcode == 0 ?? $out !! "[exit {$r.exitcode}] $err"
}

my %no-lib = %*ENV;
%no-lib<MUTSULIB>:delete;

is run-code('-e', 'print Rakudo::Internals.INCLUDE.raku', :env(%no-lib)),
    '()', 'no -I is the empty list';

is run-code('-I', $lib, '-e', 'print Rakudo::Internals.INCLUDE.raku', :env(%no-lib)),
    "(\"$lib\",)", 'a single -I is a one-element list';

is run-code('-I', $lib, '-I', 't/fixtures', '-e',
        'print Rakudo::Internals.INCLUDE.join(",")', :env(%no-lib)),
    "$lib,t/fixtures", 'several -I paths keep their order';

is run-code('-I', $lib, '-e', 'print Rakudo::Internals.INCLUDE.^name', :env(%no-lib)),
    'List', 'it is a List';

# MUTSULIB, like RAKULIB, is not a command-line option: it is not in the
# -I list (nor in %*COMPILING<%?OPTIONS><I>).
is run-code('-I', $lib, '-e', 'print Rakudo::Internals.INCLUDE.join(",")',
        :env(%(|%no-lib, MUTSULIB => 't/fixtures'))),
    $lib, 'MUTSULIB entries are not part of INCLUDE';

# Round trip: a child started with the parent's INCLUDE sees the same modules.
is run-code('-I', $lib, '-e',
        q:to/CODE/.subst("\n", ' ', :g), :env(%no-lib)),
        my @args = $*EXECUTABLE;
        for Rakudo::Internals.INCLUDE { @args.push: '-I'; @args.push: $_ }
        @args.push: '-e'; @args.push: 'use PrecProbe; print prec-probe-who()';
        print run(@args, :out).out.slurp(:close)
        CODE
    'plain', 'a forwarded INCLUDE resolves modules in a child';

ok Rakudo::Internals.IS-WIN ~~ Bool, 'IS-WIN still answers a Bool';
