use v6;
use Test;
use nqp;

plan 13;

# #10956: an Array / Hash `$!descriptor` is a live ContainerDescriptor over the
# container's own element constraint and default.
my @a = 1, 2;
is nqp::getattr(@a, Array, '$!descriptor').^name, 'ContainerDescriptor::Untyped',
    'untyped Array: Untyped descriptor';
my Int @t;
is nqp::getattr(@t, Array, '$!descriptor').of.^name, 'Int', 'typed Array: .of';
my %h;
is nqp::getattr(%h, Hash, '$!descriptor').^name, 'ContainerDescriptor::Untyped',
    'untyped Hash: Untyped descriptor, not the hash itself';
my Str %s;
is nqp::getattr(%s, Hash, '$!descriptor').of.^name, 'Str', 'typed Hash: .of';
is nqp::getattr(@t, Array, '$!descriptor').^name, 'ContainerDescriptor',
    'typed container: plain ContainerDescriptor';

# set_of writes through to the container.
my @u;
my $d = nqp::getattr(@u, Array, '$!descriptor');
$d.set_of(Str);
is @u.of.^name, 'Str', 'set_of reaches the container';
is $d.of.^name, 'Str', 'the descriptor reads the same storage';

# bindattr installs the descriptor's constraint.
my @v;
nqp::bindattr(@v, Array, '$!descriptor', nqp::getattr(@t, Array, '$!descriptor'));
is @v.of.^name, 'Int', 'bindattr of another container\'s descriptor';
my %g;
nqp::bindattr(%g, Hash, '$!descriptor', ContainerDescriptor.new(:of(Int)));
is %g.of.^name, 'Int', 'bindattr of a fresh descriptor on a Hash';
is nqp::getattr(%g, Hash, '$!descriptor').of.^name, 'Int', 'read back';

# A bind of anything else is an error, never a silent no-op.
dies-ok { nqp::bindattr(@v, Array, '$!descriptor', 42) }, 'bind of a non-descriptor dies';

# is default travels with the descriptor.
my @dd is default(7);
is nqp::getattr(@dd, Array, '$!descriptor').default, 7, '.default of an is-default Array';
is nqp::getattr(@dd, Array, '$!descriptor').^name, 'ContainerDescriptor::Untyped', 'only the element type makes it typed';
