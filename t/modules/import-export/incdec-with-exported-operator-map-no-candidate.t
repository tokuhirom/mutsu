use v6;
use Test;
use lib 'roast/packages/Test-Helpers';
use Test::Util;

plan 6;

# A module whose EXPORT hands out `multi postfix:<++>`/`prefix:<++>` candidates
# for its own type: they join the core operator's candidate set, so an ordinary
# Int still takes the core candidate.
my $dir = make-temp-dir;
$dir.add("SimOps.rakumod").spurt: q:to/END/;
    class SimAST is export {}
    multi postfix:<++>(SimAST $a) is export { 42 }
    multi prefix:<++>(SimAST $a) is export { 43 }
    END
$dir.add("Sim.rakumod").spurt: q:to/END/;
    use SimOps;
    class Sim {
        method exports(--> Map()) { flat SimOps::EXPORT::ALL:: }
    }
    multi EXPORT(+@x) { Sim.exports }
    END

sub out-of(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', $dir.Str, '-e', $code, :out, :err;
    $proc.out.slurp(:close) ~ $proc.err.slurp(:close)
}

is out-of('use Sim; my $n = 0; $n++; $n++; say $n; ++$n; say $n; $n--; say $n; my $r = $n++; say "$r $n"'),
    "2\n3\n2\n2 3\n", "++/-- on an Int after use of an EXPORT-map operator module";
is out-of('use SimOps; my $n = 0; $n++; $n++; say $n'), "2\n", "plain use of the operator module";
is out-of('use Sim; my $j = 5; $j--; say $j; our $o = 0; $o++; say $o'), "4\n1\n", "postfix -- and an our variable";
is out-of('use Sim; sub f { my $z = 0; $z++; $z }; say f()'), "1\n", "inside a sub";
is out-of('use Sim; my $m = 0; $m.++; say $m'), "1\n", "method form";
is out-of('use Sim; my @a = 1, 2; @a[0]++; say @a'), "[2 2]\n", "element increment";
