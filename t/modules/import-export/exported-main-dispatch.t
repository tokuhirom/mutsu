use v6;
use Test;

# A module that exports MAIN (`proto … is export`, as zef's `package Zef::CLI`
# does) must have its MAIN become the importing program's MAIN. A non-exported
# `sub MAIN` in a used module must NOT be auto-dispatched. The multi candidates
# need not be individually `is export` -- the proto's export covers the family.

plan 4;

my $exe = $*EXECUTABLE;

# Exported MAIN: a matching command dispatches.
{
    my $r = run($exe, '-I', 't/lib', '-e', 'use ExportedMain;', 'greet', 'world', :out, :err);
    is $r.out.slurp(:close).trim, 'hello world',
        'exported MAIN (proto is export) dispatches a matching multi candidate';
}

# Exported MAIN: a named-arg candidate dispatches.
{
    my $r = run($exe, '-I', 't/lib', '-e', 'use ExportedMain;', '--version', :out, :err);
    is $r.out.slurp(:close).trim, 'v9.9',
        'exported MAIN dispatches the :version candidate';
}

# --help shows usage on stdout, listing a subcommand.
{
    my $r = run($exe, '-I', 't/lib', '-e', 'use ExportedMain;', '--help', :out, :err);
    my $out = $r.out.slurp(:close);
    ok $out.contains('greet'), '--help lists the exported MAIN subcommands';
}

# A NON-exported `sub MAIN` in a used module must NOT be auto-dispatched.
{
    my $r = run($exe, '-I', 't/lib', '-e', 'use NonExportedMain; say "mainline";', 'x', :out, :err);
    is $r.out.slurp(:close).trim, 'mainline',
        'non-exported module MAIN is not dispatched (mainline runs instead)';
}
