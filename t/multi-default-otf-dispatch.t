use Test;

# Genuine multi candidates with a default parameter value are now OTF-compiled
# (run as compiled bytecode) instead of forced through the interpreter fallback
# (ledger §D, multi-dispatch VM-ization). The blanket default-OTF was deferred
# (#3546) because OTF-compiling a *single* sub that shadows a builtin (Test::Util
# `our sub run(Str, Str = '')`) name-caches it and mis-binds later `run` calls.
# This slice enables default-OTF only at the genuine-multi dispatch site, where
# `compile_and_call_function_def` never name-caches the candidate (the name is
# multi-cached), so that hazard cannot fire. Single / builtin-shadow candidates
# keep the default exclusion.

plan 11;

# literal default
{
    multi dm(Int $x, Int $y = 10) { $x + $y }
    multi dm(Str $s) { $s }
    is dm(5), 15, 'default applied when omitted';
    is dm(5, 20), 25, 'explicit value overrides default';
    is dm("hi"), "hi", 'other candidate still selected';
}

# default referencing an earlier positional param
{
    multi dd(Int $x, Int $y = $x * 2) { $x + $y }
    multi dd(Str $s) { $s }
    is dd(5), 15, 'default references earlier param';
    is dd(5, 1), 6, 'explicit overrides param-referencing default';
}

# default closing over an outer lexical
{
    my $base = 100;
    multi ee(Int $x, Int $y = $base) { $x + $y }
    multi ee(Str $s) { $s ~ "!" }
    is ee(1), 101, 'default closes over outer lexical';
    is ee(1, 2), 3, 'explicit overrides closure default';
    is ee("hey"), "hey!", 'string candidate still selected';
}

# recursion through a default-bearing multi
{
    multi fact(Int $n, Int $acc = 1) { $n <= 1 ?? $acc !! fact($n - 1, $acc * $n) }
    is fact(5), 120, 'recursion with an accumulator default';
}

# narrower (no-default) candidate wins for an exact-arity call
{
    multi gg(Int $x, Int $y = 7) { "first:" ~ ($x + $y) }
    multi gg(Int $x) { "second:$x" }
    is gg(3), "second:3", 'exact-arity candidate preferred over default-bearing one';
    is gg(3, 0), "first:3", 'default-bearing candidate selected for 2-arg call';
}
