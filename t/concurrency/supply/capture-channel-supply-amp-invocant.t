use Test;

plan 11;

# Channel.Capture drains the channel to a list: Pair elements become nameds,
# the rest positionals.
{
    my $c = Channel.new;
    $c.send: $_ for |<a b c>, :42z;
    $c.close;
    is-deeply $c.Capture, \('a', 'b', 'c', :42z), 'Channel.Capture';
}

# Supply.Capture taps the supply (running an on-demand `supply { }` block) and
# collects its emissions.
is-deeply (supply { .emit for |<a b c>, :42z }).Capture, \('a', 'b', 'c', :42z),
    'Supply.Capture';

# A List holding a non-Str Pair key stringifies the key via its `.Str` (a
# custom-class key uses its `method Str`).
is-deeply (class { method Str { 'foo' } } => 42,).Capture, \(:foo(42)),
    'List with custom-class Pair key .Capture';

is-deeply (<10> => <20>, 30 => 40).Capture, ('10' => <20>, '30' => 40).Capture,
    'List with allomorph/numeric Pair keys .Capture';

# A `.&sub(...)` call binds its invocant positionally, even when the invocant is
# a literal colonpair (which would otherwise splat into a named argument).
{
    sub f(\what, $d = 'def') { "{what.raku}|$d" }
    is :42foo.&f('Pair'), ':foo(42)|Pair', 'literal colonpair invocant is positional';

    my $p = :42foo;
    is $p.&f('Pair'), ':foo(42)|Pair', 'Pair-valued variable invocant is positional';

    # A Pair-valued invocant that does reach a real named param still works.
    sub g(\what, :$tag) { "{what.raku}|{$tag // 'none'}" }
    is :42foo.&g(:tag<x>), ':foo(42)|x', 'invocant positional, explicit named separate';
}

# `.&method` invocant that is NOT a Pair is unaffected.
{
    sub h(\what) { what * 2 }
    is 21.&h, 42, 'Int invocant via .& unaffected';
    is 'ab'.&({ $^x ~ $^x }), 'abab', 'Str invocant via .& block unaffected';
}

# `.&` invocant with a mutating sub (CallMethodDynamicMut path).
{
    my @log;
    sub note-it(\what) { @log.push: what.raku; what }
    :1a.&note-it;
    is-deeply @log, [':a(1)'], 'mutating .& path keeps Pair invocant positional';
}

# A regular method call on a Pair is unaffected (sanity).
is :42foo.key, 'foo', 'plain method call on a Pair still works';
