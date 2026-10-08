use Test;

# From LWP::Simple (t/get-unsized.t): `$blob.item` yields a Scalar around the
# Blob; `~`/`~=` must decontainerize it and concatenate bytes like rakudo.
plan 5;

my $c = Buf.new(6).item;
$c ~= Buf.new(1);
is $c.raku, 'Buf.new(6,1)', 'itemized Buf ~= Buf concatenates bytes';

my Blob $r = Buf.new(5, 6, 7);
sub parse(Blob $b) { return "s", $b.subbuf(1).item }
my ($s, $content) = parse($r);
$content ~= $r;
is $content.raku, 'Buf.new(6,7,5,6,7)', 'subbuf(..).item ~= Blob';

is (Buf.new(1).item ~ Buf.new(2)).raku, 'Buf.new(1,2)', 'itemized on the left';
is (Buf.new(1) ~ Buf.new(2).item).raku, 'Buf.new(1,2)', 'itemized on the right';
dies-ok { my $x = Buf.new(1).item ~ "a" }, 'Buf ~ Str still dies';
