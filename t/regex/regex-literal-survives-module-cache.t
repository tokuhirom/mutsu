use Test;

# A regex literal in a module keeps its parser source tree in the compiled
# module cache (#18003), so a warm load matches through the same path as a cold
# one. The same program runs with the bytecode cache off, then on twice (the
# second run is served from the cache), and must print rakudo's answers each
# time.

plan 3;

my $lib = $?FILE.IO.parent(2).add('lib').Str;
my $program = q:to/PROG/;
    use RegexLiteralCache;
    say versions(<libfoo.so.1 libreadline.so.8 LIBREADLINE.SO.7 libbar.so.2>);
    say words('ab ab cd');
    say adverbed('-XYYz');
    PROG

my $expected = "8,7\n2,1\nYY\n";

for 0, 1, 1 -> $cache {
    my $proc = run $*EXECUTABLE, '-I', $lib, '-e', $program, :out, :err,
        env => %*ENV.clone.append((MUTSU_PRECOMP_BYTECODE => $cache));
    is $proc.out.slurp, $expected,
        "regex literals from a module match alike with the bytecode cache {$cache ?? 'on' !! 'off'}";
}
