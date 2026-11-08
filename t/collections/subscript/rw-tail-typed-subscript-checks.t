use Test;

plan 6;

# The container-mode subscript of an `is rw` tail runs the same key / bounds
# checks as a plain read (#17133).
my %h{Int}; %h{1} = 2;
sub f() is rw { %h{'foo'} }
throws-like { my $x = f() }, X::TypeCheck::Binding::Parameter,
    'a non-conformant key on a typed-key hash dies through an rw tail';

sub ok-key() is rw { %h{1} }
is ok-key(), 2, 'a conformant key reads';
ok-key() = 5;
is %h{1}, 5, 'and assigns through the tail';

my int @arr[5];
sub g() is rw { @arr[10] }
throws-like { my $x = g() }, X::AdHoc, 'an out-of-range index of a shaped array dies';

sub ok-idx() is rw { @arr[2] }
ok-idx() = 7;
is @arr[2], 7, 'an in-range shaped element assigns through the tail';

my %plain = a => 1;
sub p() is rw { %plain<new> }
p() = 3;
is %plain<new>, 3, 'an untyped hash still creates the key on assignment';
