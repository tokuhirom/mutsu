use Test;

# A module's mainline compiles as a module unit whether or not the compiled
# bytecode cache serves it (ADR-11756): with the cache off, its top-level
# `use`s still fill the module's lexical import sets and bind the variables
# and types they import in its pad (ADR-8751, #14140). The same program runs
# with the cache on and off and must print the same, rakudo's, answers.

plan 2;

my $lib = $?FILE.IO.parent(2).add('lib').Str;
my $program = q:to/PROG/;
    my $mub-value = 'caller';
    my $mub-ro = 'caller';
    use ModuleUnitBind::Writer;
    use ModuleUnitBind::Reader;
    use ModuleUnitBind::Readonly;
    my &c = writer-closure();
    {
        my $mub-value = 'shadow';
        say c();
    }
    say writer-reads();
    say reader-reads();
    say ro-reads();
    $mub-ro = 'reassigned';
    say "$mub-value $mub-ro";
    say (try EVAL 'MubClass') // 'not visible';
    PROG

my $expected = "written/hi\nwritten/a,b\nwritten\nbound\ncaller reassigned\nnot visible\n";

for 1, 0 -> $cache {
    my $proc = run $*EXECUTABLE, '-I', $lib, '-e', $program, :out, :err,
        env => %*ENV.clone.append((MUTSU_PRECOMP_BYTECODE => $cache));
    is $proc.out.slurp, $expected,
        "the module's imports bind in its pad with the bytecode cache {$cache ?? 'on' !! 'off'}";
}
