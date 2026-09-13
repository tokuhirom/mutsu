use Test;

plan 8;

# #8294: a bare `::` NOT glued to `(` is the null routine/method name (this
# routine has no name), not an attempt at the indirect-declarator-name form
# `::(EXPR)` (glued). Verified against rakudo.

{
    my $m = anon sub :: ($x) { $x + 1 };
    is $m(41), 42, 'anon sub :: ($x) {...} is a nameless sub taking the following signature';
}

{
    # `anon method :: (Window: ...) {...}` is #8294's actual repro (found in
    # UI::HTMLWindow's `&routine.wrap(anon method :: (Window: *@_, *%_) {...})`).
    # Only the parse (and that the built callable runs) is asserted here --
    # whether `anon method (...)` binds the invocant like a real Method is a
    # separate, pre-existing gap (mutsu reports `.^name` as `Sub`, same as the
    # already-nameless `anon method (Window: *@a) {...}` form) and is not
    # this ticket's concern.
    class Window { }
    sub call_it(&r) { r(Window, 1, 2) }
    is call_it(anon method :: (Window: *@a) { @a.join(",") }),
        call_it(anon method (Window: *@a) { @a.join(",") }),
        'anon method :: (invocant: sig) {...} parses and runs identically to the already-nameless anon method (invocant: sig) {...} form';
}

{
    my $ran = False;
    my $s = sub :: () { $ran = True };
    $s();
    ok $ran, 'sub :: () {...} (space, empty signature) builds a callable nameless sub';
}

{
    # A `::` glued to `(` is still the pre-existing indirect-declarator-name
    # form and must keep working exactly as before.
    my constant sname = 'null_decl_indirect_sub';
    sub ::(sname) ($x) { $x + 1 }
    is null_decl_indirect_sub(41), 42, 'sub ::(name) (...) {...} (glued) still declares a callable sub';
}

{
    class M {
        method ::('sp ace') { 23 }
    }
    is M."sp ace"(), 23, 'method ::("...") (glued) still allows names with spaces';
}

{
    # Plain nameless forms (no `::` at all) are unaffected.
    my $m = anon sub ($x) { $x + 1 };
    is $m(41), 42, 'anon sub ($x) {...} (no ::) is unaffected';
}

{
    class W2 { method (*@a) { @a.elems } }
    lives-ok { W2 }, 'a nameless method (*@a) {...} inside a class body (no ::) still parses';
}

{
    my $s = sub :: ($a, $b) { $a + $b };
    is $s(3, 4), 7, 'sub :: ($a, $b) {...} with a space reads (...) as a real two-param signature, never as an indirect-name expression';
}
