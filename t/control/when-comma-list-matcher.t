use Test;

plan 3;

my $whatever-curry;
given 1, 2 {
    when * == 1, * {
        $whatever-curry = 'matched';
    }
}
is $whatever-curry, 'matched',
    'a Whatever-curry comma-list matcher matches a list topic';

my $bare-whatever;
given 1, 2 {
    when *, * {
        $bare-whatever = 'matched';
    }
}
is $bare-whatever, 'matched',
    'a bare Whatever comma-list matcher matches a list topic';

my $literal-list;
given 1, 2 {
    when 1, 2 {
        $literal-list = 'matched';
    }
}
is $literal-list, 'matched',
    'a literal comma-list matcher matches a list topic';
