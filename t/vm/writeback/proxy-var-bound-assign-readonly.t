use Test;

# A variable bound to the Proxy object `.VAR` hands back has no container to
# assign into: Rakudo dies with the readonly-variable error and never calls the
# Proxy's STORE (#16299).

plan 5;

my $stores = 0;
my $q := Proxy.new(
    FETCH => method ()   { 1 },
    STORE => method ($v) { $stores++ },
);

my $b := $q.VAR;
throws-like { $b = 5 }, X::AdHoc, message => /'readonly variable'/,
    'assigning to a variable bound to .VAR dies';
is $stores, 0, '... and STORE did not run';

# A plain Proxy binding still stores through, and assignment of .VAR keeps
# its own value-holding semantics.
my $p := Proxy.new(
    FETCH => method ()   { 1 },
    STORE => method ($v) { $stores++ },
);
$p = 7;
is $stores, 1, 'a variable bound to a Proxy calls STORE';

my $v = $q.VAR;
$v = 5;
is $stores, 1, 'a variable that was assigned .VAR just replaces it';
is $v, 5, '... and holds the new value';
