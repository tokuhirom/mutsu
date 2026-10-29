use v6;
use Test;
use lib 'roast/packages/Test-Helpers';
use Test::Util;

plan 3;

# Net::BGP: an attribute `where` closure of a class declared in a module calls
# a proto/multi family the module `use`d. Its visibility is per compunit, so
# the closure must run in the declaring file's unit, not the caller's.
my $dir = make-temp-dir;
$dir.add("AwChk.rakumod").spurt: q:to/END/;
    unit module AwChk;
    our subset Quad of Str where / ^ [\d+] ** 4 % '.' $ /;
    our proto chk(Str:D $ip --> Bool:D) is export {*};
    multi chk(Quad:D $ip --> Bool:D) { True }
    multi chk(Str:D $ip --> Bool:D) { False }
    END
$dir.add("AwPeer.rakumod").spurt: q:to/END/;
    unit class AwPeer;
    use AwChk;
    has Str:D $.ip is required where { chk($^a) };
    has Str $.alt where { chk($^a) } = "9.9.9.9";
    END

sub out-of(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', $dir.Str, '-e', $code, :out, :err;
    $proc.out.slurp(:close) ~ $proc.err.slurp(:close)
}

is out-of('use AwPeer; say AwPeer.new(:ip("1.2.3.4")).ip'), "1.2.3.4\n",
    'a valid address passes the attribute where';
like out-of('use AwPeer; AwPeer.new(:ip("nope"))'), /'where constraint failed'/,
    'an invalid address still fails it';
is out-of('use AwPeer; say AwPeer.new(:ip("1.2.3.4"), :alt("5.6.7.8")).alt'), "5.6.7.8\n",
    'a non-required attribute where';
