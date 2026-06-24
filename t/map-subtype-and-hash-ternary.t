use Test;

# Two general fixes surfaced while loading the Humming-Bird web framework's
# request/response core (Humming-Bird::Glue).

plan 9;

# 1. Hash IS-A Map in raku's type hierarchy, so a Hash satisfies Map / Map:D.
my %h = a => 1;
ok %h ~~ Map, 'a Hash smartmatches Map';
ok %h ~~ Map:D, 'a defined Hash smartmatches Map:D';
lives-ok { my sub f(--> Map:D) { my %x = b => 2; %x }; f() },
    'returning a Hash satisfies a `--> Map:D` constraint';
my sub g(Map:D $m) { $m<a> }
is g(%h), 1, 'a Hash binds to a Map:D parameter';

# ...but Pair/Set/Bag are NOT Map subtypes (they only do Associative).
nok (a => 1) ~~ Map, 'a Pair is not a Map';
nok set(1, 2) ~~ Map, 'a Set is not a Map';

# 2. A ternary (and other complex expression) as a comma-separated element in a
#    `{ }` hash literal parses (raku flattens it into the initializer).
my $f = 'file.txt';
my %headers = ct => 'text';
my %part = {
    :%headers,
    $f ?? :$f !! (),
    body => 'B',
};
is %part<f>, 'file.txt', 'ternary element yielding a colonpair lands in the hash';
is %part<body>, 'B', 'pairs around the ternary element are preserved';
is %part<headers><ct>, 'text', 'colonpair-shorthand element is preserved';
