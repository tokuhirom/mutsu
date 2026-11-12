use Test;
use nqp;

# A Hash wrapper made with `nqp::create(Hash)` whose `$!storage` is bound to a
# storage another hash already uses writes into that shared storage (#16888,
# EEL 0.3's `Event.EEL-hash` view of a capture's `%!hash`).

plan 11;

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

# The storage read inline, as EEL 0.3's `EEL-hash` does: every view of the
# capture, old or new, names the capture's one store (#17413).
class InlineEv is Capture is Any {
    method live-hash {
        my $hash := nqp::create(Hash);
        nqp::bindattr(nqp::decont($hash), Map, '$!storage', nqp::getattr(self, Capture, '%!hash'));
        $hash
    }
}
my $f = InlineEv.new(:hash({x => 1}));
my %first := $f.live-hash;
my %second := $f.live-hash;
%second<z> = 3;
is %first<z>, 3, 'an earlier view sees a write through a later one';
is $f<z>, 3, 'and so does the capture';
%first<w> = 4;
is %second<w>, 4, 'a later view sees a write through an earlier one';
is $f<w>, 4, 'and so does the capture';

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
