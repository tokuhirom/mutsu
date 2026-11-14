use Test;
use nqp;

# An IO::CatHandle's `&!on-switch` may hold a Proxy: IO::CatHandle::AutoLines
# binds one with `nqp::bindattr` so its line counter resets per file. Reading
# the attribute to call it FETCHes the Proxy, so the callable it hands back
# runs on every handle switch.

plan 2;

my $dir = $*TMPDIR.add("mutsu-cathandle-on-switch-proxy-$*PID");
$dir.mkdir;
my $f1 = $dir.add('f1.txt');
my $f2 = $dir.add('f2.txt');
$f1.spurt("a\nb\n");
$f2.spurt("c\nd\n");

role LineCount {
    has Int:D $.ln is rw = 0;
    has &!store;
    submethod TWEAK {
        sub reset { $!ln = 0 }
        &!store := &reset;
        nqp::bindattr(self, IO::CatHandle, '&!on-switch', Proxy.new:
            :FETCH{ &!store }, :STORE(-> $, &code { }));
    }
    method get {
        my \v = callsame;
        v === Nil ?? ($!ln = 0) !! ++$!ln;
        v
    }
}

my $h = IO::CatHandle.new($f1, $f2) does LineCount;
my @seen = (^4).map: { $h.get ~ ':' ~ $h.ln };
is @seen.join(' '), 'a:1 b:2 c:1 d:2', 'the Proxy-held on-switch resets the count per file';

my $switches = 0;
my $h2 := IO::CatHandle.new($f1, $f2);
nqp::bindattr($h2, IO::CatHandle, '&!on-switch',
    Proxy.new(:FETCH{ -> { $switches++ } }, :STORE(-> $, $ { })));
$h2.lines.eager;
is $switches, 2, 'each later switch, including exhaustion, calls the fetched callable';

$f1.unlink; $f2.unlink; $dir.rmdir;
