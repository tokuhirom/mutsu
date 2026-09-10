use Test;

# Raku: a routine (sub/method) gets its own `$_` initialized to `(Any)` — it
# does NOT inherit the caller's topic. Regression: the positional/named "light"
# call paths read `$_` through the scoped env overlay, which fell through to the
# caller's `given`/`for`/`with` topic, so a param-taking sub that reads `$_` saw
# the caller's topic value instead of a fresh `Any`. (The no-param path and the
# method path already installed a fresh `$_`.) Fixed by shadowing `$_` with Any
# at the routine boundary, gated on the body actually referencing `$_`
# (`reads_topic`) so a topic-free hot loop keeps its fast path.

plan 13;

sub noparam()          { $_.^name }
sub oneparam($x)       { $_.^name }
sub typedparam(Int $x) { $_.^name }
sub twoparam($x, $y)   { $_.^name }
sub namedparam($x, :$opt) { $_.^name }
class C { method m($x) { $_.^name } }
my $c = C.new;

# Baseline at top level: $_ is Any.
is noparam(),  'Any', 'no-param sub: $_ is Any at top level';
is oneparam(1), 'Any', 'one-param sub: $_ is Any at top level';

# Inside a `given` with a Str topic, the routine's own $_ is still Any.
given 'ctx' {
    is noparam(),        'Any', 'no-param sub: fresh $_ inside given';
    is oneparam(1),      'Any', 'one-param sub: fresh $_ inside given';
    is typedparam(1),    'Any', 'typed-param sub: fresh $_ inside given';
    is twoparam(1, 2),   'Any', 'two-param sub: fresh $_ inside given';
    is namedparam(1, opt => 2), 'Any', 'named-param sub: fresh $_ inside given';
    is $c.m(1),          'Any', 'method: fresh $_ inside given';
}

# Inside a `for` loop (element topic).
my @names;
for 'a', 'b' {
    @names.push(oneparam(1));
}
is @names.join(','), 'Any,Any', 'one-param sub: fresh $_ inside for loop';

# Inside `with`.
with 'w' {
    is oneparam(1), 'Any', 'one-param sub: fresh $_ inside with';
}

# A routine that WRITES its $_ then reads it back sees the written value, not
# the caller topic.
sub setread($x) { $_ = $x * 2; $_ }
given 'ctx' {
    is setread(5), 10, 'routine that assigns $_ reads back its own value';
}

# An explicit `$_` parameter binds the topic to the argument (not Any, not the
# caller topic).
sub explicit($_) { "got:$_" }
given 'ctx' {
    is explicit('arg'), 'got:arg', 'explicit $_ param binds the argument';
}

# The caller's topic is unchanged after the call.
my $after = '';
given 'ctx' {
    oneparam(1);
    $after = $_;
}
is $after, 'ctx', "caller's topic intact after a fresh-\$_ routine call";
