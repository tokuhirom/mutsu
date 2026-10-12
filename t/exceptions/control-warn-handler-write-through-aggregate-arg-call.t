use Test;

plan 4;

# A CONTROL handler resumed inline at a deep `warn` must still leave its write
# to the installing closure's lexical visible after the intervening frames
# return, also when a frame passes an aggregate variable to a scalar parameter.

sub warner($x) { warn "U4"; 1 }
sub via-hash   { my %d = a => 1; warner %d }
sub via-array  { my @d = 1;      warner @d }
sub via-scalar { my $d = 1;      warner $d }

sub run(&body) { body() }

my $got-hash = "";
run({
    my $m = "";
    CONTROL { when CX::Warn { $m = .message; .resume } }
    via-hash();
    $got-hash = $m;
});
is $got-hash, "U4", "hash variable passed to a scalar parameter";

my $got-array = "";
run({
    my $m = "";
    CONTROL { when CX::Warn { $m = .message; .resume } }
    via-array();
    $got-array = $m;
});
is $got-array, "U4", "array variable passed to a scalar parameter";

my $got-scalar = "";
run({
    my $m = "";
    CONTROL { when CX::Warn { $m = .message; .resume } }
    via-scalar();
    $got-scalar = $m;
});
is $got-scalar, "U4", "scalar variable passed to a scalar parameter";

my $got-assigned = "";
my &blk = {
    my $m = "";
    CONTROL { when CX::Warn { $m = .message; .resume } }
    via-hash();
    $got-assigned = $m;
};
blk();
is $got-assigned, "U4", "closure stored in a variable";

done-testing;
