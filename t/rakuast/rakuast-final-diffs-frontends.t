use Test;

my @files = <
    t/lang/parsing/parser-statement-continues-past-early-stop.t
    t/lang/quoting/double-angle-quote-word-lt.t
    t/modules/import-export/use-inline-proto-argument.t
    t/modules/import-export/use-trace-pragma.t
    t/routines/dispatch/core-infix-user-candidate-fallbacks.t
    t/routines/signature/placeholder-scope-signature-capable.t
    t/types/enum-subset/enum.t
    t/vm/codegen/adr0116-trir-peephole.t
    t/vm/nqp-p6-frame-state-ops.t
    t/vm/sigilless-term-infix-pipe.t
>;

plan @files.elems;

my %env = %*ENV;
%env<MUTSU_RAKUAST> = '1';
for @files -> $file {
    my $proc = run($*EXECUTABLE, $file, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    is $proc.exitcode, 0, "$file passes through the RakuAST frontend"
        or diag "$out\n$err";
}
