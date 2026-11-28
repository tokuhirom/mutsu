# `IndexUpvalue` (ADR-18637's fused `GetUpvalue` + `Index`) is on the Tier A
# JIT's step list, so a hot loop reading a captured array through it is
# compiled instead of the whole range bailing out to the interpreter (#19406).
#
# The script reads `:=`-bound (celled) arrays from `start` blocks -- the shape
# that enables the snapshot read -- while one worker also writes the array it
# reads, and checks the post-`await` values. It runs with the JIT on and off,
# and the two transcripts must agree.
use Test;

plan 4;

my $code = q:to/RAKU/;
    my @data = ^256;
    my @alias := @data;
    my $seed = 7;
    my $sref := $seed;
    my @readers = (^4).map: -> $t {
        start {
            my $s = 0;
            for ^20000 -> $i { $s += @alias[$i +& 255] + $sref }
            $s
        }
    };
    say "readers: ", (await @readers).join(",");

    my @backing = 0 xx 64;
    my @slots := @backing;
    my @mixed = (^4).map: -> $t {
        start {
            my $s = 0;
            for ^5000 -> $i {
                @slots[$t * 16 + ($i +& 15)] = $t * 1000 + $i;
                $s += @slots[$t * 16 + ($i +& 15)];
            }
            $s
        }
    };
    say "mixed: ", (await @mixed).join(",");
    say "slots: ", [+] @slots;

    # Non-integer and out-of-range subscripts take the general path.
    my @words = <a b c>;
    my @w := @words;
    say "words: ", (await start { (^6).map({ @w[$_ % 4] // "-" }).join }).Str;
    RAKU

sub transcript(%extra-env) {
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, '-e', $code, :out, :err, :%env);
    ($proc.exitcode, $proc.out.slurp(:close), $proc.err.slurp(:close))
}

my ($on-code, $on-out, $on-err) = transcript({ MUTSU_VM_STATS => '1' });
my ($off-code, $off-out, $off-err) = transcript({ MUTSU_JIT => 'off' });

is $on-code, 0, 'runs clean with the JIT on' or diag $on-err;
is $on-out, $off-out, 'JIT on and off agree' or diag $off-err;
is $on-out.lines.join("|"),
    'readers: 2686416,2686416,2686416,2686416|mixed: 12497500,17497500,22497500,27497500|'
        ~ 'slots: 415456|words: abc-ab',
    'the transcript carries the expected answers';

my $bail = $on-err.lines.first(*.contains('jit bailout opcodes')) // '';
nok $bail.contains('IndexUpvalue'), "the JIT does not decline IndexUpvalue ($bail)";
