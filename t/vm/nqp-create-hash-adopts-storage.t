use Test;
use nqp;

# A Hash wrapper made with `nqp::create(Hash)` whose `$!storage` is bound to a
# storage another hash already uses writes into that shared storage (#16888,
# EEL 0.3's `Event.EEL-hash` view of a capture's `%!hash`).

plan 7;

sub view-of(%h) {
    my $hash := nqp::create(Hash);
    my $storage := nqp::getattr(nqp::decont(%h), Map, '$!storage');
    nqp::bindattr(nqp::decont($hash), Map, '$!storage', $storage);
    $hash
}

my %owner = a => 1;
my %view := view-of(%owner);
%view<b> = 2;
is-deeply %owner.keys.sort, <a b>, 'a write through the view reaches the owner';
%owner<c> = 3;
is-deeply %view.keys.sort, <a b c>, 'a write to the owner is seen by the view';
is %view<a>, 1, 'the view reads the shared entries';

class Ev is Capture {
    method set-captures(Hash:D $captures) {
        my $storage := nqp::getattr(nqp::decont($captures), Map, '$!storage');
        nqp::bindattr(self, Capture, '%!hash', $storage);
        self
    }
    method view-hash {
        my $hash := nqp::create(Hash);
        my $storage := nqp::getattr(self, Capture, '%!hash');
        nqp::bindattr(nqp::decont($hash), Map, '$!storage', $storage);
        $hash
    }
    method add($name, $value) {
        my %hash := self.view-hash;
        %hash{$name} = $value;
        self
    }
}

my %caps = a => 1;
my $e = Ev.new.set-captures(%caps);
$e.add('data', 5);
is-deeply %caps.keys.sort, <a data>, "a capture's hash view writes the shared captures";
%caps<b> = 2;
is-deeply $e.hash.keys.sort, <a b data>, 'and the capture still shares them';

# A target someone else holds keeps receiving the storage (#12672).
sub fill(%h) {
    my $r := nqp::hash;
    nqp::bindattr(%h, Map, '$!storage', $r);
    nqp::bindkey($r, 'z', 2);
    %h<z>
}
my %q;
is fill(%q), 2, 'a held target sees the storage it was given';
is %q<z>, 2, 'and so does its holder';
