use Test;

# Prompt::Hidden t/01-plain.t: `prompt` answers val($line), so a numeric-looking
# line is an allomorph, and end of input is Nil (as for `get`).

plan 6;

my $exe = $*EXECUTABLE;

sub name-for(Str $input, Str $code = 'print prompt("").^name') {
    my $p = run $exe, '-e', $code, :in, :out;
    $p.in.print($input);
    $p.in.close;
    my $o = $p.out.slurp(:close);
    $o;
}

is name-for("1234\n"), 'IntStr', 'an integer line is an IntStr';
is name-for("0x1f\n"), 'IntStr', 'a hex line is an IntStr';
is name-for("1.5\n"), 'RatStr', 'a decimal line is a RatStr';
is name-for("abc\n"), 'Str', 'a non-numeric line is a plain Str';
is name-for(""), 'Nil', 'end of input is Nil';
is name-for("42\n", 'my $n = prompt(""); print $n + 1'), '43', 'the allomorph numifies';

done-testing;
