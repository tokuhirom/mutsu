use Test;
plan 8;

# Test 1: Basic MAIN with string arg
{
    my $result = run($*EXECUTABLE, '-e', 'sub MAIN(Str $name) { say "Hello, $name" }', 'World', :out, :err);
    is $result.out.slurp(:close).trim, "Hello, World", "MAIN with positional Str arg";
}

# Test 2: MAIN sub is called automatically (no args case)
{
    my $result = run($*EXECUTABLE, '-e', 'sub MAIN() { say "called" }', :out, :err);
    is $result.out.slurp(:close).trim, "called", "MAIN sub with no args is called";
}

# Test 3: Usage message on missing required args (single print)
{
    my $result = run($*EXECUTABLE, '-e', 'sub MAIN(Str $name) { say "Hello, $name" }', :out, :err);
    my $err = $result.err.slurp(:close);
    my $count = $err.comb("Usage:").elems;
    is $count, 1, "Usage message printed exactly once";
}

# Test 4: --help shows usage on stdout
{
    my $result = run($*EXECUTABLE, '-e', 'sub MAIN(Str $name) { say "Hello, $name" }', '--help', :out, :err);
    my $out = $result.out.slurp(:close);
    ok $out.contains("Usage:"), "--help shows Usage on stdout";
    ok $out.contains("<name>"), "--help shows parameter name";
}

# Test 5: Exit code 0 on success
{
    my $result = run($*EXECUTABLE, '-e', 'sub MAIN(Str $name) { say "Hello, $name" }', 'World', :out, :err);
    is $result.exitcode, 0, "Exit code 0 on successful MAIN dispatch";
}

# Test 6: Exit code non-zero on usage error
{
    my $result = run($*EXECUTABLE, '-e', 'sub MAIN(Str $name) { say "Hello, $name" }', :out, :err);
    ok $result.exitcode != 0, "Non-zero exit code on usage error";
}

# Test 7: multi MAIN dispatch with Int coercion
{
    my $code = 'multi sub MAIN("greet", Str $name) { say "Hello, $name" }; multi sub MAIN("add", Int $a, Int $b) { say $a + $b }';
    my $result = run($*EXECUTABLE, '-e', $code, 'add', '3', '5', :out, :err);
    is $result.out.slurp(:close).trim, "8", "multi MAIN with Int coercion";
}
