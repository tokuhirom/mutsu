use Test;

# Three separate defects in how a coercion parameter `T(F)` takes part in
# dispatch and binding, all found while re-measuring the `XML` battery
# (`todo/tickets/bundle-xml-battery.md`); `XML` spells its entry point
#
#     multi sub open-xml (IO::Path(Str) $src where :f) { ... }
#     multi sub open-xml (Str $src)                    { ... }
#     multi sub open-xml (IO::Handle $src)             { ... }
#
# and mutsu answered the plain-`Str` candidate for a path string, then failed
# outright for an `IO::Path` argument.

plan 17;

# --- 1. `T.new($value)` completes the coercion protocol for a NATIVE class ---
# Rakudo's coercion protocol ends in `TargetType.new($value)`. mutsu only tried
# it for classes in its own class registry with a user-written `new`, so every
# native target (`IO::Path`) raised X::Coerce::Impossible.
{
    sub takes-path(IO::Path(Str) $p) { $p.^name }
    is takes-path('/etc/hostname'), 'IO::Path',
        'a Str coerces to a native IO::Path parameter';
}
{
    sub takes-path(IO::Path(Str) $p) { $p.Str }
    is takes-path('/etc/hostname'), '/etc/hostname',
        'the coerced value carries the original string';
}
# A USER class with no positional `new` still cannot be coerced into — the
# behaviour rakudo has, and the reason the fallback is gated on the target
# declaring no user `new` at all.
{
    my class Bare { has $.v }
    sub takes-bare(Bare(Str) $b) { $b.^name }
    dies-ok { takes-bare('x') },
        'a user class with no positional new is still an impossible coercion';
}
{
    my class Made { has $.v; method new($v) { self.bless(:$v) } }
    sub takes-made(Made(Str) $m) { $m.v }
    is takes-made('x'), 'x', 'a user class with a positional new still coerces';
}

# --- 2. a `where` on a coercion parameter runs against the COERCED value ---
# `IO::Path(Str) $src where :f` means "coerce to IO::Path, then ask it `.f`".
# mutsu ran the predicate against the raw Str, where `.f` does not exist, so
# the candidate was silently rejected at dispatch time.
{
    multi sub only-file(IO::Path(Str) $p where :f) { 'file:' ~ $p.^name }
    multi sub only-file(IO::Handle $p) { 'handle' }
    is only-file('/etc/hostname'), 'file:IO::Path',
        'the where predicate sees the coerced IO::Path';
}
{
    multi sub pick(IO::Path(Str) $p where :f) { 'path' }
    multi sub pick(Str $p) { 'str' }
    is pick('/etc/hostname'), 'path',
        'the where-constrained coercion candidate wins for an existing file';
    is pick('this-file-does-not-exist-98765'), 'str',
        'and yields to the plain Str candidate when the predicate fails';
}

# --- 3. a coercion parameter accepts the TARGET type directly ---
# Rakudo compiles `T(F)` into two candidates, one taking `T` itself. mutsu only
# accepted `F`, so an `IO::Path` argument matched no candidate at all.
{
    multi sub pick2(IO::Path(Str) $p) { 'path:' ~ $p.^name }
    multi sub pick2(IO::Handle $p) { 'handle' }
    is pick2('/etc/hostname'.IO), 'path:IO::Path',
        'an IO::Path binds the IO::Path(Str) parameter without coercion';
    is pick2('/etc/hostname'), 'path:IO::Path',
        'and a Str still coerces into it';
}
{
    sub num-or-str(Int(Str) $n) { $n }
    is num-or-str('42'), 42, 'Int(Str) still coerces a Str';
    is num-or-str(7), 7, 'Int(Str) accepts an Int directly';
}

# --- 4. a junction argument bound by a SLURPY is not auto-threaded ----------
# `method m(*%q)` slurps its named arguments as raw values, so a Junction is
# stored whole. mutsu auto-threaded it, calling the method once per eigenstate
# and handing back `any(result, result)` — which is how
# `XML::Element.lookfor(:class(Nil | "skip"))` answered one junction instead of
# the two matching elements.
{
    my $calls = 0;
    my class C {
        method slurpy-hash(*%q) { $calls++; %q<k> }
        method slurpy-pos(*@a) { $calls++; @a.elems }
        method plain($x) { $calls++; $x }
        method named(:$k) { $calls++; $k }
    }
    my $c = C.new;

    $calls = 0;
    my $h = $c.slurpy-hash(:k(1 | 2));
    is $calls, 1, 'a slurpy hash parameter is called once, not per eigenstate';
    ok $h ~~ Junction, 'and receives the junction whole';

    $calls = 0;
    my $p = $c.slurpy-pos(1 | 2);
    is $calls, 1, 'a slurpy positional parameter is called once too';
    is $p, 1, 'and collects the junction as a single element';

    # The shapes that DO auto-thread must keep doing so.
    $calls = 0;
    $c.plain(1 | 2);
    is $calls, 2, 'a plain positional parameter still auto-threads';

    $calls = 0;
    $c.named(:k(1 | 2));
    is $calls, 2, 'a declared named parameter still auto-threads';
}
