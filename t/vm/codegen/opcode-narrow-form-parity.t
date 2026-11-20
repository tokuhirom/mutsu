use Test;

# Every narrow op answers what its generic op answers (ADR-18357 D6): the
# same program runs once as compiled and once with `MUTSU_NARROW_OPS=0`,
# which makes every narrow form take the generic op, and the two outputs are
# compared line by line.

plan 2;

my $program = q:to/END/;
    sub loop-count(int $n) { my int $p = 0; $p++ while $p < $n; $p }
    say loop-count(257);
    {
        my int $p = 5;
        say $p++, ' ', $p, ' ', ++$p, ' ', $p--, ' ', --$p;
    }
    {
        my int $p = 9223372036854775807;
        $p++;
        say $p;
        $p--;
        say $p;
        my int $q = -9223372036854775808;
        say $q--, ' ', $q;
    }
    {
        my int $p = 0;
        my &bump = { $p++ };
        bump() for ^3;
        $p++;
        say $p;
    }
    {
        my int $i = 0;
        my int $t = 0;
        while $i < 20 { $t += $i; ++$i }
        say $t;
    }
    {
        my int $p = 3;
        say $p++ + $p++, ' ', $p;
    }
    END

sub run-with(Str $setting) {
    my %env = %*ENV;
    %env<MUTSU_NARROW_OPS> = $setting;
    my $proc = run $*EXECUTABLE, '-e', $program, :out, :err, :%env;
    $proc.out.slurp(:close) ~ $proc.err.slurp(:close);
}

my $narrow = run-with('1');
my $generic = run-with('0');
ok $narrow.lines.elems > 5, 'the program ran';
is $narrow, $generic, 'the narrow and the generic ops print the same';
