use Test;

# ADR-0040 §9.2: a renderer resolves its receiver's `Proxy` elements before
# rendering. Rakudo renders a container by calling `.gist`/`.Str`/`.raku` on each
# element and a method call deconts its invocant, so a `Proxy` element renders as
# its FETCHed value — at render time, with the container still holding the Proxy
# afterwards.
#
# Closes `todo/tickets/list-element-proxy-not-rendered-through-fetch.md`, whose
# table of rakudo 2026.06 answers every expectation below is taken from.

plan 28;

# A fresh Proxy over `$cell`, plus the cell itself so a test can move it.
sub make-proxy($cell is rw) {
    Proxy.new(FETCH => -> $ { $cell }, STORE => -> $, $v { $cell = $v });
}

# 1. A List keeps its Proxy (its elements are not containers, so §9's store
#    FETCH deliberately does not apply), and all six renderers FETCH it.
{
    my $n = 5;
    my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    my $l = (1, $p, 3);

    is $l.gist,       '(1 5 3)',   'List .gist FETCHes a Proxy element';
    is $l.Str,        '1 5 3',     'List .Str FETCHes a Proxy element';
    is $l.raku,       '$(1, 5, 3)','List .raku FETCHes a Proxy element';
    is $l.join(','),  '1,5,3',     'List .join FETCHes a Proxy element';
    is "$l",          '1 5 3',     'interpolation FETCHes a Proxy element';
    is ~$l,           '1 5 3',     'prefix ~ FETCHes a Proxy element';

    # The FETCH happens at RENDER time, not at construction: the list still
    # holds the Proxy, so a later change to the backing lexical shows through.
    $n = 9;
    is $l.gist, '(1 9 3)', 'the Proxy is still in the list and re-FETCHes';

    # And the element is still a container, so a store reaches its STORE.
    $l[1] = 7;
    is $n, 7, 'storing into the element fires the Proxy STORE, not a replacement';
    is $l.gist, '(1 7 3)', 'and the next render shows the stored value';
}

# 2. `say`/`put`/`print`/`note` render the same way. (Checked through a Proc so
#    the child's own output is the assertion, keeping `is-run`-free.)
{
    my $n = 5;
    my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    my $l = (1, $p, 3);
    my $out = '';
    {
        my $*OUT = class { method print(*@a) { $out ~= @a.join('') } }.new;
        say $l;
        put $l;
        print $l;
    }
    is $out, "(1 5 3)\n1 5 3\n1 5 3", 'say/put/print all FETCH a Proxy element';
}

# 3. §9.1's element bind is the other way a Proxy legitimately sits inside a
#    container — an Array or a Hash this time, with the Proxy behind the
#    element's own container cell. Every renderer looks through it.
{
    my $n = 5;
    my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    my @a = 1, 2, 3;
    @a[1] := $p;

    is @a.gist,      '[1 5 3]',    'a bound Array element FETCHes for .gist';
    is @a.raku,      '[1, 5, 3]',  'a bound Array element FETCHes for .raku';
    is @a.join(','), '1,5,3',      'a bound Array element FETCHes for .join';
    is ~@a,          '1 5 3',      'a bound Array element FETCHes for prefix ~';
    is "@a[]",       '1 5 3',      'a bound Array element FETCHes for interpolation';
    is @a[1].VAR.^name, 'Proxy',   'and the element is still the Proxy container';

    my %h;
    %h<k> := $p;
    is %h.gist, '{k => 5}',   'a bound Hash value FETCHes for .gist';
    is %h.raku, '{:k(5)}',    'a bound Hash value FETCHes for .raku';
}

# 4. A method that hands the element to USER code is deliberately not a
#    renderer: it binds the element container, Proxy included (ADR-0045).
{
    my $n = 5;
    my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    my $l = (1, $p, 3);
    is $l.map({ $_ + 1 }).join(','), '2,6,4',
        'map still sees the element (through its own read), and renders FETCHed';
    is $l.map({ ($_ > 2).Str }).join(','), 'False,True,True',
        'and a comparison inside the block reads the FETCHed value';
    # `grep` over a Proxy element is a separate, pre-existing defect --
    # `todo/tickets/grep-drops-a-proxy-element-from-its-result.md`.
}

# 5. A Proxy whose FETCH returns an object with a user-defined `.Str` composes:
#    the FETCH runs first, then that class's stringifier.
{
    my class Boxed { method Str { 'BOXED' } method gist { 'GIST' } }
    my $b = Boxed.new;
    my $p := Proxy.new(FETCH => -> $ { $b }, STORE => -> $, $ { });
    my $l = (1, $p, 3);
    is $l.join(','), '1,BOXED,3', 'FETCH then the class Str, for .join';
    is ~$l,          '1 BOXED 3', 'FETCH then the class Str, for prefix ~';
    is $l.gist,      '(1 GIST 3)', 'FETCH then the class gist, for .gist';
}

# 6. Nesting: a Proxy inside a list inside a list still resolves.
{
    my $n = 5;
    my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    my $l = (1, (2, $p), 3);
    is $l.gist, '(1 (2 5) 3)', 'a Proxy nested two levels deep FETCHes';
}

# 7. A container with no Proxy in it is untouched — the scan is a fast no-op and
#    must not perturb ordinary rendering, itemization included.
{
    my $plain = (1, (2, 3), 4);
    is $plain.gist, '(1 (2 3) 4)', 'a Proxy-free list renders exactly as before';
    is $plain.raku, '$(1, (2, 3), 4)', 'including its itemization in .raku';
}

# 8. A container is allowed to hold itself, and the Proxy scan now runs on user
#    data at every render — so it must stop at a node it has already visited
#    rather than walking the cycle forever. Without this the process aborted with
#    a stack overflow on the first circular array it rendered.
{
    my @circ;
    @circ = 42, @circ;
    ok @circ.raku.chars, 'a circular array still renders (the Proxy scan is cycle-safe)';
    # `.gist` on a circular array is a SEPARATE, pre-existing stack overflow in
    # the gist renderer's own walk (`.raku` has cycle detection, `.gist` does
    # not) -- `todo/tickets/gist-of-a-circular-container-overflows-the-stack.md`.

    my %ch;
    my @cb;
    %ch = :b(%ch), :c(@cb);
    @cb = %ch, @cb, 42;
    ok @cb.raku.chars, 'a circular array within a circular hash likewise';
}

# vim: expandtab shiftwidth=4
