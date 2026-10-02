use Test;

plan 2;

# `@a[^2]=@o` (no spaces) is a subscript then `=`, not a `[^2]=` reduction meta-assign.
my @a = 0 xx 5;
my @o = 7, 8;
@a[^2]=@o;
is @a.gist, '[7 8 0 0 0]', '@a[^2]=@o slice-assigns';

my $n = 3;
my @b = 0 xx 5;
@b[^$n]=1, 2, 3;
is @b.gist, '[1 2 3 0 0]', '@b[^$n]=list slice-assigns';
