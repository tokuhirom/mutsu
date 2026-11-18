use v6;
use Test;

# A module's exported `proto MAIN` imported inside a block (or a sub) is
# lexical to it: when the program ends, its own `MAIN` runs, not the module's
# proto. From rakudoc's t/01-cmd.t, which tests `Rakudoc::CMD::MAIN` from a
# sub that does `use Rakudoc::CMD`, next to the script's own
# `BEGIN sub MAIN(|) { }`. mutsu ran the module's proto body at the end and died
# with "Cannot resolve caller MAIN()". Expected output checked against raku.

plan 4;

my $exe = $*EXECUTABLE;

sub run-script(Str $code, *@args) {
    my $r = run($exe, '-I', 't/lib', '-e', $code, |@args, :out, :err);
    ($r.exitcode, $r.out.slurp(:close).trim, $r.err.slurp(:close).trim)
}

{
    my ($exit, $out, $err) = run-script q:to/CODE/;
        sub MAIN(|) { say "program MAIN" }
        sub f { use BlockScopedMainProto; BlockScopedMainProto::MAIN("x") }
        f();
        CODE
    is $out, "module proto: module MAIN x\nprogram MAIN",
        'a proto MAIN imported in a sub leaves the program MAIN to run at the end';
    is $exit, 0, '... and the program exits cleanly';
}

{
    my ($exit, $out, $err) = run-script q:to/CODE/;
        BEGIN sub MAIN(|) { };
        { use BlockScopedMainProto; }
        say "mainline";
        CODE
    is-deeply ($exit, $out, $err), (0, "mainline", ""),
        'a proto MAIN imported in a bare block is gone when the block ends';
}

{
    my ($exit, $out, $err) = run-script q:to/CODE/, 'q';
        use BlockScopedMainProto;
        CODE
    is $out, "module proto: module MAIN q",
        'a unit-level import still makes the module MAIN the program MAIN';
}
