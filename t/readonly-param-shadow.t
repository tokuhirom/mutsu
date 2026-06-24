use Test;

# A caller's readonly parameter must NOT make a same-named `my` variable in a
# CALLEE (or a later inner scope) read-only. `my $x` always introduces a fresh,
# writable binding. Before the fix, a readonly param `$x` in an outer routine
# leaked into a callee's `my $x`, raising "Cannot assign to a readonly variable".

plan 7;

# --- the core bug: readonly param leaks into a callee's `my` of the same name ---
sub callee() {
    my $body = 'init';
    $body = 'changed';
    return $body;
}
sub outer(Str $body) {        # readonly positional param named $body
    return callee();
}
is outer("ro"), 'changed', 'callee my-var with caller readonly-param name is writable';

# --- destructuring + //= (the shape that surfaced this in a web framework) ---
sub parse(Str $s) {
    my ($a, $body) = $s.split('|', 2);
    $body //= 'DEF';
    return "$a:$body";
}
sub wrapper(:$body = '') {     # readonly named param $body
    return parse('x|y') ~ ' ' ~ parse('only');
}
is wrapper(), 'x:y only:DEF', 'destructure + //= works under a readonly-named param caller';

# --- nested calls, same name three deep ---
sub deep3() { my $x = 1; $x = $x + 1; $x }
sub deep2(Int $x) { deep3() }
sub deep1(Int $x) { deep2(99) }
is deep1(7), 2, 'same-named readonly params three frames deep do not block inner my';

# --- the readonly param itself is STILL readonly (no over-correction) ---
sub ro-stays(Str $z) {
    my $err;
    { $z = 'nope'; CATCH { default { $err = .message } } }
    return $err.defined;
}
ok ro-stays("x"), 'a readonly param remains readonly (assignment still throws)';

# --- `is copy` param is writable as before ---
sub with-copy(Str $w is copy) { $w = 'w2'; $w }
is with-copy("w1"), 'w2', 'is copy param stays writable';

# --- a plain my then reassign at top level still works ---
my $top = 'a';
$top = 'b';
is $top, 'b', 'top-level my reassignment works';

# --- a `:=` literal-bound scalar is STILL readonly (the unmark must skip binds) ---
dies-ok {
    my $bound := 5;
    $bound = 6;
}, 'a literal-bound (`:=`) scalar stays readonly even though it is a `my` decl';
