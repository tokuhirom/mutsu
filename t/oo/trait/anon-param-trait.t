use Test;

# A type-only (anonymous) parameter may still carry a trait (`is rw`, `is copy`)
# or a `where` clause — previously mutsu's signature parser only accepted a
# trailing `)`, `,`, `]`, `{`, `-->` after the type and choked on `is`/`where`.
# This shape is common in NativeCall signatures, e.g. `(Str, Pointer is rw)`.

plan 9;

# --- parses, and `is rw` is enforced like Rakudo (writable arg required) ---
{
    sub f(Str, Int is rw) returns Int { 42 }
    my $n = 1;
    is f("x", $n), 42, 'anon `is rw` param accepts a writable variable';
    dies-ok { f("x", 1) }, 'anon `is rw` param rejects a literal (X::Parameter::RW)';
}

# --- multiple anon params, mixed traits ---
{
    sub g(Int is rw, Str is copy) { 'ok' }
    my $i = 1;
    is g($i, "a"), 'ok', 'two anon params with traits parse and run';
}

# --- anon param with a where clause ---
{
    sub h(Int where * > 0) { 'pos' }
    is h(5), 'pos', 'anon param with a where clause parses';
    dies-ok { h(-1) }, 'the where clause on the anon param is enforced';
}

# --- mixed: named param after an anon-with-trait ---
{
    sub k(Int is rw, Str $name) { $name }
    my $i = 1;
    is k($i, "bob"), 'bob', 'named param follows an anon-with-trait param';
}

# --- a bare type-only param (no trait) still works (no regression) ---
{
    sub m(Int) { 'typed' }
    is m(7), 'typed', 'plain type-only param still parses';
}

# --- `is rw` on a *named* param is unaffected ---
{
    sub n($x is rw) { $x = 99 }
    my $v = 1;
    n($v);
    is $v, 99, 'is rw on a named param still writes back';
}

# --- anon param trait coexists with an arrow return type ---
{
    sub p(Int is rw --> Int) { 3 }
    my $i = 1;
    is p($i), 3, 'anon-with-trait coexists with an arrow return type';
}
