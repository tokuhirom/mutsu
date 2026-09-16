use Test;

# A method (or sub) whose signature fixes the return value (`--> Nil`, and
# the same family of definite return specs) sinks its body's actual tail
# value before substituting the fixed one -- Raku semantics, and what mutsu
# already did for a `sub`. Method bodies took a completely separate compile
# path (`compile_method_body`) that never consulted `return_type` at all, so
# the method-call path only replicated PART of the sink at runtime
# (`Interpreter::sink_for_definite_return`): it knew about `LazyList` and any
# value whose elements were already reified, but ADR-0058 made `.map`/`.grep`
# return a `Seq` that is deliberately NOT reified until first consumption
# (`SeqSource::MapGrep`) -- a plain `Deref` read (what the reified-elements
# fallback used) sees the empty seed and never runs the callback.
#
# Reduced from `Game::Entities` 0.1.6's `multi method delete($guid, *@c
# where *.elems > 1 --> Nil) { @c.map: { $.delete: $guid, $_ } }`: the
# multi-component delete silently deleted nothing at all (github.com/
# tokuhirom/mutsu#8496).

plan 4;

my @seen;

class Foo {
    method f(--> Nil) {
        (1, 2, 3).map: { @seen.push($_) };
    }
}
Foo.new.f;
is @seen, (1, 2, 3), 'a bare tail .map in a --> Nil method body runs its callback';

@seen = ();
sub g(--> Nil) {
    (4, 5).map: { @seen.push($_) };
}
g();
is @seen, (4, 5), 'the equivalent sub already worked and still does';

@seen = ();
class Bar {
    method h(--> True) {
        (6, 7).map: { @seen.push($_) };
    }
}
my $r = Bar.new.h;
is $r, True, '--> True still substitutes the fixed value';
is @seen, (6, 7), '...after sinking the real tail value first';
