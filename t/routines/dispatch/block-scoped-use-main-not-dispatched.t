use Test;

# A `MAIN` a module exports is imported lexically: `{ use Mod; }` must not leave
# it behind as the program's own MAIN (CLI::Ecosystem's t/01-basic.rakutest
# relies on this "separate scope to prevent normal MAIN processing" idiom; the
# stray dispatch ran the whole tool after the test finished).

plan 4;

my $dir = $*TMPDIR.add("mutsu-block-use-main-{$*PID}-{(^1000000).pick}");
$dir.mkdir;
LEAVE { .unlink for $dir.dir; $dir.rmdir }

$dir.add("MainMod.rakumod").spurt(q:to/EOS/);
    unit module MainMod;
    multi sub MAIN(Bool :$help) is export { say "MAIN ran" }
    EOS
$dir.add("MainProto.rakumod").spurt(q:to/EOS/);
    unit module MainProto;
    proto sub MAIN(|) is export {*}
    multi sub MAIN(Bool :$help) { say "proto MAIN ran" }
    EOS

sub run-script(Str $code) {
    my $script = $dir.add("script.raku");
    $script.spurt($code);
    my $proc = run $*EXECUTABLE, '-I', $dir.Str, $script.Str, :out, :err;
    $proc.out.slurp(:close)
}

is run-script("\{ use MainMod; \}\nsay 'done';\n"), "done\n",
    "multi MAIN imported inside a block is not dispatched";
is run-script("\{ use MainProto; \}\nsay 'done';\n"), "done\n",
    "proto MAIN imported inside a block is not dispatched";
is run-script("use MainMod;\nsay 'done';\n"), "done\nMAIN ran\n",
    "a top-level use still imports MAIN as the program's MAIN";
is run-script("use MainProto;\nsay 'done';\n"), "done\nproto MAIN ran\n",
    "a top-level use of a proto MAIN still dispatches it";
