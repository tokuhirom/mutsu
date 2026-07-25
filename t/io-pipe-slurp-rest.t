use Test;

plan 9;

# IO::Pipe inherits `slurp-rest` from IO::Handle: it reads from the current
# cursor to the end of the captured output. `slurp` behaves the same way.

my $exe = $*EXECUTABLE;

{
    my $proc = run :out, $exe, '-e', 'say 1; say 2';
    is $proc.out.slurp-rest(:close), "1\n2\n", 'slurp-rest reads the whole output';
}

{
    my $proc = run :out, $exe, '-e', 'say 1; say 2';
    my $pipe = $proc.out;
    is $pipe.slurp-rest(:close), "1\n2\n", 'slurp-rest (first call) drains the pipe';
    is $pipe.slurp-rest, '', 'slurp-rest after draining returns the empty string';
}

{
    my $proc = run :out, $exe, '-e', 'say 1; say 2; say 3';
    is $proc.out.get, '1', '.get reads one line';
    is $proc.out.slurp-rest(:close), "2\n3\n", 'slurp-rest returns only the remainder';
}

{
    my $proc = run :out, $exe, '-e', 'say 1; say 2; say 3';
    is $proc.out.get, '1', '.get reads one line (slurp variant)';
    is $proc.out.slurp(:close), "2\n3\n", 'slurp is cursor-aware too';
}

{
    my $proc = run :out, $exe, '-e', 'say 1';
    my $buf = $proc.out.slurp-rest(:bin, :close);
    is $buf.list, (49, 10), 'slurp-rest(:bin) returns the raw bytes';
}

{
    my $proc = run :out, :err, $exe, '-e', 'note 9';
    is $proc.err.slurp-rest(:close), "9\n", 'slurp-rest works on the err pipe';
}

# vim: expandtab shiftwidth=4
