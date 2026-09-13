use Test;

plan 14;

# A brace is classified by what is between the braces, never by what follows
# the `}`. `f { "a" => 1 }` composed a Hash, but the *same* braces in
# `f { "a" => 1 }, @rest` were claimed by the `name { block }, args` listop
# shape and became a Block -- so JSON::Fast's own `to-json($obj, :sorted-keys)`
# suite died with "Don't know how to jsonify Block".

sub takes-two($obj, @keys) { $obj }
sub takes-one($obj) { $obj }
sub takes-named($obj, @keys, :$message) { ($obj, $message) }

# The regression: a hash composer followed by a second argument.
isa-ok takes-two({ "a" => 1 }, <x y>), Hash,
    'a quoted-key hash composer stays a hash with a second argument';
my $no-paren = takes-two { "a" => 1 }, <x y>;
isa-ok $no-paren, Hash, 'the no-paren listop spelling composes a hash too';
isa-ok takes-two({ a => 1 }, <x y>), Hash,
    'a bareword-key hash composer stays a hash with a second argument';
isa-ok takes-two({ "a" => { "b" => 1 } }, <x y>), Hash,
    'a nested hash composer stays a hash with a second argument';
isa-ok takes-two({}, <x y>), Hash,
    'empty braces stay a hash with a second argument';

# The same braces with nothing after them were always right; pin both spellings
# so the two can never disagree again.
isa-ok takes-one({ "a" => 1 }), Hash, 'a hash composer as the only argument';
is-deeply takes-two({ "a" => 1, "b" => 2 }, <x y>), {a => 1, b => 2},
    'the hash keeps its contents through the call';

# A named argument after the list is the `assert-sorted` shape from
# JSON::Fast's t/08-sorted-keys.t.
my ($obj, $msg) = takes-named { "aaaa" => { "aaac" => 1 } }, <aaaa aaac>,
    message => "nested";
isa-ok $obj, Hash, 'a hash composer before a trailing named argument';
is $msg, 'nested', 'the trailing named argument still binds';

# Blocks must stay blocks: the listop block-argument shape is what this path
# exists for.
is map({ $_ * 2 }, (1, 2, 3)).List, (2, 4, 6).List, 'map takes a block, not a hash';
is (map { $_ * 2 }, (1, 2, 3)).List, (2, 4, 6).List, 'the no-paren spelling too';
is (grep { $_ > 1 }, (1, 2, 3)).List, (2, 3).List, 'grep takes a block';
is (sort { $^a <=> $^b }, (3, 1, 2)).List, (1, 2, 3).List,
    'a placeholder body is a block, not a hash';
is (map { %( x => $_ ) }, (1, 2)).map(*.<x>).List, (1, 2).List,
    'a block whose body builds a hash is still a block';
