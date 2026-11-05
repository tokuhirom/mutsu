use Test;

# `.VAR` of a Proxy is the Proxy object itself. A hash value and a list element
# keep it; an array element FETCHes it, as Rakudo does (#15893).

plan 7;

my $fetches = 0;
my $q := Proxy.new(
    FETCH => method ()   { $fetches++; 1 },
    STORE => method ($v) { },
);

my %h;
%h<k> = $q.VAR;
is %h<k>.^name, 'Proxy', 'a hash value keeps the Proxy object';
is $fetches, 0, '... without a FETCH';

my @l = ($q.VAR,);
is @l[0].^name, 'Proxy', 'a list element keeps the Proxy object';
is $fetches, 0, '... without a FETCH';

my @a;
@a[1] = $q.VAR;
is @a[1].^name, 'Int', 'an array element FETCHes it';
is $fetches, 1, '... once';

my %p;
%p<k> = $q;
is %p<k>, 1, 'a plain Proxy stored in a hash still FETCHes';
