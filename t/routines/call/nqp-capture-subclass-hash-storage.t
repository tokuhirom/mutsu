use Test;
use nqp;

# A user `Capture` subclass's `%!hash` is a raw hash nqp code shares with a
# `Hash` (EEL's `Event`, #16888): installing a Hash's `$!storage` there makes
# the capture's nameds that hash, and installing the capture's `%!hash` into a
# fresh Hash makes the Hash write into the capture.

plan 10;

class Ev is Capture is Any {
    method install(Hash:D $captures) {
        my $storage := nqp::getattr(nqp::decont($captures), Map, '$!storage');
        nqp::bindattr(self, Capture, '%!hash', $storage);
        self
    }
    method live-hash {
        my $hash := nqp::create(Hash);
        my $storage := nqp::getattr(self, Capture, '%!hash');
        unless nqp::ishash($storage) {
            $storage := nqp::hash;
            nqp::bindattr(self, Capture, '%!hash', $storage);
        }
        nqp::bindattr(nqp::decont($hash), Map, '$!storage', $storage);
        $hash
    }
    method positions(@values) {
        my $list = @values.List;
        nqp::bindattr(self, Capture, '@!list', nqp::getattr(nqp::decont($list), List, '$!reified'));
        self
    }
}

my %c;
my $e = Ev.new(:hash(%c)).install(%c);
%c<a> = 1;
is $e<a>, 1, 'a write to the installed Hash is seen by the capture';
is $e.hash, Map.new((a => 1)), 'and by its .hash';

my $f = Ev.new(:hash({x => 1}));
my %h := $f.live-hash;
is %h<x>, 1, 'the Hash holds what the capture held';
%h<y> = 2;
is $f<y>, 2, 'a write through the Hash is seen by the capture';
is $f.hash.elems, 2, 'its .hash has both';
ok $f<y>:exists, ':exists on the new key';
is-deeply $f<missing>, Nil, 'a missing key is Nil';

my %g := $f.live-hash;
%g<z> = 3;
is $f<z>, 3, 'a second Hash over the capture writes into it too';

$f.positions([7, 8]);
is $f.list, (7, 8), '@!list installs the positionals';
is $f<x>, 1, 'and keeps the nameds';
