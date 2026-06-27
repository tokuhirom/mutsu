use Test;

plan 4;

sub make-adder($x) {
    -> $y { $x + $y }
}

my $add = make-adder(10);
my $x = 999;
is $add(5), 15, 'closure reads captured lexical, not caller same-name lexical';

sub make-counter() {
    my $n = 0;
    -> { $n++ }
}

my $next = make-counter();
my $n = 100;
is $next(), 0, 'first call sees captured counter state';
is $next(), 1, 'captured counter state persists across calls';
is $n, 100, 'caller lexical with same name is untouched';
