use Test;

plan 8;

class CloneBase { has $.y is rw }
class CloneChild is CloneBase { }
class OwnClone is CloneBase { method clone(|) { 'own clone' } }

my $method = CloneBase.^find_method('clone');
is $method.package.^name, 'Mu', 'the inherited native method retains its declaring owner';
ok !$method.is-wrapped, 'clone starts unwrapped';

my $handle = $method.wrap(-> $self, |c {
    my $copy = callsame;
    $copy.y ~= '!';
    $copy
});
ok $method.is-wrapped, 'the inherited method reports its wrap';
is CloneBase.new(:y<a>).clone.y, 'a!', 'the wrapper reaches the base class';
is CloneChild.new(:y<a>).clone(:y<b>).y, 'b!', 'the wrapper reaches a child class';
is OwnClone.new(:y<a>).clone, 'own clone', 'a user override wins over the native wrap';

$handle.restore;
ok !$method.is-wrapped, 'restoring the handle removes the wrap';
is CloneBase.new(:y<a>).clone.y, 'a', 'clone runs normally after restore';
