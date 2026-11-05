use Test;

plan 5;

class Col is IO::Handle {
    has @.got;
    method WRITE(Blob:D \data --> Bool:D) { @!got.push: data.decode; True }
}
my $b1 = Col.new(:!chomp); $b1.encoding('utf8');
my $b2 = Col.new(:!chomp); $b2.encoding('utf8');
my $b3 = Col.new(:!chomp); $b3.encoding('utf8');

{ my $*OUT = $*ERR; $*OUT := $b1; say "block"; }
is-deeply $b1.got, ["block\n"], 'a rebind of my $*OUT in a mainline block redirects say';

sub s { my $*OUT = $*ERR; $*OUT := $b2; say "sub"; }
s();
is-deeply $b2.got, ["sub\n"], 'the routine form redirects say';

{ my $*OUT = $*ERR; $*OUT := $b1; print "p"; put "q"; }
is-deeply $b1.got, ["block\n", "p", "q\n"], 'print and put follow the rebind';

{ my $*OUT = $*ERR; $*OUT := $b3; $*OUT.say("explicit"); }
is-deeply $b3.got, ["explicit\n"], 'the explicit method form still works';

say "after";
is $b3.got.elems, 1, 'the binding ends with the block';
