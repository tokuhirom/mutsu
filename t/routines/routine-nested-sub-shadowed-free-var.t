use Test;

# A `my sub` declared inside a routine reads the free variables of the
# routine activation that declared it, not whatever same-named local its
# caller has (mutsu#9111).

plan 14;

sub sh($p) { my sub t() { $p }; -> { my $p = 99; t() } }
is sh(5)(), 5, 'closure declaring a same-named local calls the inner sub';

my ($a, $b) = sh(1), sh(2);
is "{$a()} {$b()}", '1 2', 'each activation keeps its own binding';

sub nb($p) { my sub t() { $p }; { my $p = 9; t() } }
is nb(4), 4, 'call from a nested block that shadows the variable';

sub fl($p) { my sub t() { $p }; my @r; for 1..2 -> $p { @r.push: t() }; @r.join(',') }
is fl(7), '7,7', 'call from a for loop whose parameter shadows the variable';

sub b3($x) { my sub t() { $x }; (1..3).map(-> $x { t() }).join(',') }
is b3('X'), 'X,X,X', 'call from a map callback whose parameter shadows it';

sub wr() {
    my $c = 0;
    my sub bump() { $c++ }
    my $cl = -> { my $c = 100; bump(); bump(); $c };
    my $r = $cl();
    "$r $c"
}
is wr(), '100 2', 'a write reaches the declaring variable and spares the shadow';

sub rb() { my $c = 1; my sub t() { $c = 5 }; my $s; { my $c = 9; t(); $s = $c }; "$s $c" }
is rb(), '9 5', 'assignment from a shadowing block';

sub a1($p) { my sub t() { (1, 2).map({ $p + $_ }).join(',') }; { my $p = 100; t() } }
is a1(10), '11,12', 'closure created inside the inner sub captures its binding';

sub a2() { my @a = 1, 2; my sub t() { @a.elems }; -> { my @a = 1..9; t() } }
is a2()(), 2, 'array free variable';

sub a3() { my @a; my sub t($x) { @a.push: $x }; { my @a = 7; t(1); t(2) }; @a.join(',') }
is a3(), '1,2', 'array mutation through the inner sub under a shadow';

sub a4($p) { my sub t() { my sub u() { $p }; -> { my $p = 0; u() } }; t()() }
is a4(42), 42, 'inner sub nested two levels deep';

sub a5() { my $x = 1; my sub t() { $x }; $x = 2; -> { my $x = 0; t() } }
is a5()(), 2, 'a later assignment in the routine stays visible';

sub rec($n) { my sub g() { $n }; my $r = $n > 0 ?? rec($n - 1) !! ''; "$r" ~ g() }
is rec(3), '0123', 'recursion keeps one binding per activation';

sub esc($p) { my sub t() { $p }; &t }
sub callit(&f) { my $p = 'caller'; f() }
is callit(esc(30)), 30, 'an escaped &t ignores its caller\'s same-named local';
