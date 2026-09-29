use v6;
use Test;

# An `is export` routine declared inside another routine's body is exported
# at compile time (mutsu#10050): the importer can call it without the
# enclosing routine having run first. Called during the enclosing routine's
# dynamic extent, its free variables resolve to that routine's live frame.

plan 4;

my $lib = $*TMPDIR.add("mutsu-nested-export-{$*PID}");
$lib.mkdir;
LEAVE { try { .unlink for $lib.dir; $lib.rmdir } }

$lib.add('NestedExport.rakumod').spurt(q:to/END/);
    unit module NestedExport;
    multi sub set(Callable $c) is export {
        my @t;
        my multi sub test(Str $d, Callable $s) is export { @t.push($d) }
        my multi sub test(Callable $s) is export { test("anon", $s) }
        $c();
        @t.join(",")
    }
    sub counter(&body) is export {
        my $n = 0;
        my sub bump($by = 1) is export(:count) { $n += $by }
        body();
        $n
    }
    END

sub run-code($code) {
    my $proc = run($*EXECUTABLE, '-I', $lib.Str, '-e', $code, :out, :err);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($out, $err)
}

{
    my ($out, $err) = run-code(q:to/CODE/);
        use NestedExport;
        say set(sub { test("a", sub {}); test(sub {}) });
        say set(sub { test("b", sub {}) });
        CODE
    is $out, "a,anon\nb\n", 'nested multi sub is export is callable and closes over the live frame';
    is $err, '', 'no error';
}

{
    my ($out, $err) = run-code(q:to/CODE/);
        use NestedExport :DEFAULT, :count;
        say counter({ bump; bump(5) });
        CODE
    is $out, "6\n", 'nested plain sub is export(:tag) is imported by its tag';
    is $err, '', 'no error';
}
