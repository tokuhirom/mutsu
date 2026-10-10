use Test;

# `--dump-ast` and `--dump-bytecode` parse without executing (#11212). Two
# parse-time probes used to run a `use`d module's mainline anyway: the dynamic
# EXPORT-stash probe (#9500) and slang activation (ADR-0026). Each fixture
# module leaves a marker file when its mainline runs; dumping must not create
# it, and a real run still must.

plan 12;

my $dir = $*TMPDIR.add("mutsu-dump-ast-no-execute-$*PID");
$dir.mkdir;
LEAVE { try { .unlink for $dir.dir; $dir.rmdir } }

my $dyn-marker = $dir.add('dyn-marker');
$dir.add('DynNoExec.rakumod').spurt: qq:to/END/;
    '{$dyn-marker.absolute}'.IO.spurt('ran');
    my package EXPORT::DEFAULT \{
        for <a b> -> \$c \{ OUR::\{'&postfix:<' ~ \$c ~ 'Z>'\} := sub (\$n) \{ \$n \} \}
    \}
    END

my $slang-marker = $dir.add('slang-marker');
$dir.add('SlangNoExec.rakumod').spurt: qq:to/END/;
    '{$slang-marker.absolute}'.IO.spurt('ran');
    role SlangNoExec \{ token term-now \{ nowish \} \}
    my sub EXPORT \{
        \$*LANG.define_slang('MAIN', \$*LANG.slang_grammar('MAIN').^mixin(SlangNoExec));
        BEGIN Map.new
    \}
    END

my $dyn-main = $dir.add('dyn-main.raku');
$dyn-main.spurt: "use lib '{$dir.absolute}';\nuse DynNoExec;\nsay 1;\n";
my $slang-main = $dir.add('slang-main.raku');
$slang-main.spurt: "use lib '{$dir.absolute}';\nuse SlangNoExec;\nsay 1;\n";

sub mutsu(*@args) {
    my $proc = run $*EXECUTABLE.absolute, |@args, :out, :err;
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    ($proc.exitcode, $out)
}

my $trait-marker = $dir.add('trait-marker');
my $trait-main = $dir.add('trait-main.raku');
$trait-main.spurt: qq:to/END/;
    multi trait_mod:<is>(Routine \$r, :\$tagged!) \{
        '{$trait-marker.absolute}'.IO.spurt('trait');
    \}
    proto sub tagged-proto(|) is tagged('{$trait-marker.absolute}'.IO.spurt('argument')) \{*\}
    multi sub tagged-proto(Int) \{ 1 \}
    END

for '--dump-ast', '--dump-bytecode' -> $flag {
    my ($code, $out) = mutsu($flag, $trait-main.absolute);
    is $code, 0, "$flag succeeds on a proto with an executable trait argument";
    nok $trait-marker.e, "$flag executes neither the proto trait nor its argument";
}

for '--dump-ast', '--dump-bytecode' -> $flag {
    my ($code, $out) = mutsu($flag, $dyn-main.absolute);
    is $code, 0, "$flag succeeds on a module with a computed export stash";
    nok $dyn-marker.e, "$flag does not run a module to learn its computed exports";
}

{
    my ($code, $out) = mutsu('--dump-ast', $slang-main.absolute);
    is $code, 0, '--dump-ast succeeds on a slang-activating module';
    nok $slang-marker.e, '--dump-ast does not run a module to activate its slang';
}

{
    my ($code, $out) = mutsu($dyn-main.absolute);
    ok $dyn-marker.e && $out eq "1\n", 'a real run still loads the computed-export module';
}

{
    my ($code, $out) = mutsu($slang-main.absolute);
    ok $slang-marker.e && $out eq "1\n", 'a real run still activates the slang';
}

# vim: expandtab shiftwidth=4
