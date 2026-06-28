use Test;

# A single user sub with a default parameter that SHADOWS a builtin is now
# OTF-compiled (was excluded to avoid an otf_call_cache name-cache hazard, PR
# #3546). The hazard is closed by the cache's `fn_resolve_gen` scope coherence:
# when the shadowing scope exits the cache is evicted, so the builtin re-binds.
# (PLAN §2-D.)

plan 5;

# User `run` (default param) shadowing the builtin `run`, then the builtin
# `run` (process) outside the shadowing scope.
{
    my $inside;
    {
        sub run(Str $code, Str $input = '') { "user:$code/$input" }
        $inside = run("a");
    }
    is $inside, "user:a/", "user run with default binds inside its scope";

    my $proc = run("echo", "hello", :out);
    is $proc.out.slurp(:close).chomp, "hello",
        "builtin run (process) re-binds after the shadowing scope exits";
}

# A user sub shadowing a builtin, both calls in the same scope, default applied.
{
    sub run(Str $code, Str $tag = 'def') { "$code\[$tag]" }
    is run("x"), "x[def]", "default applied to shadowing sub (no arg)";
    is run("x", "y"), "x[y]", "explicit arg to shadowing sub";
}

# A non-shadowing single sub with a default still works (regression guard).
{
    sub greet(Str $who, Str $greeting = "Hi") { "$greeting, $who" }
    is greet("Sam"), "Hi, Sam", "ordinary default-param sub";
}
