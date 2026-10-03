use Test;

plan 6;

# Every element write into a native num32 array rounds to single precision.
my $f = 0.1e0.Num;
my num32 @n = 0.1e0;
my $single = @n[0];
isnt $single, $f, 'initial assignment rounds to single precision';

@n[0] = 0.1e0;
is @n[0], $single, 'direct element store rounds';

@n.push(0.1e0);
is @n[1], $single, 'push rounds';

@n.unshift(0.1e0);
is @n[0], $single, 'unshift rounds';

@n.append(0.1e0, 0.1e0);
is @n[3], $single, 'append rounds';

@n[7] = 0.1e0;
is @n[7], $single, 'store past the end rounds';
