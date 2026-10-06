use Test;

# An empty `Map`'s `.raku` is the bare `Map.new`: Rakudo drops the argument
# list when there are no pairs. mutsu rendered `Map.new(())`, which only
# `.gist` (and so `say`) keeps. A non-empty Map is `Map.new((:a(1)))` in both.
# Expected values come from `raku`.

plan 21;

# The reported repro.
is Map.new.raku, 'Map.new', 'Map.new.raku';
is %().Map.raku, 'Map.new', '%().Map.raku';
{
    my %h;
    is %h.Map.raku, 'Map.new', 'an empty %h.Map.raku';
}
is ("a" ~~ /a/).hash.raku, 'Map.new', 'a match with no named captures: .hash.raku';

# The `.gist` / `say` form keeps its empty argument list.
is Map.new.gist, 'Map.new(())', 'Map.new.gist keeps the empty list';
{
    my $m = Map.new;
    is $m.gist, 'Map.new(())', 'a scalar-held empty Map .gist';
    is $m.raku, '$(Map.new)', 'a scalar-held empty Map is itemized in .raku';
}
is (Map.new,).raku, '(Map.new,)', 'an empty Map inside a List .raku';
is (Map.new,).gist, '(Map.new(()))', 'an empty Map inside a List .gist';

# Non-empty Maps are unchanged.
is Map.new((:a(1))).raku, 'Map.new((:a(1)))', 'one pair';
is Map.new((:a(1), :b(2))).raku, 'Map.new((:a(1),:b(2)))', 'two pairs';
is Map.new((:a(1))).gist, 'Map.new((a => 1))', 'a non-empty Map .gist';

# A Map held in a typed container goes through its own renderer.
{
    my Map $e = Map.new;
    is $e.raku, '$(Map.new)', 'a Map-typed container, empty';
    is $e.gist, 'Map.new(())', 'a Map-typed container, empty .gist';
    my Map $m = Map.new(:a(1), :b(2));
    is $m.raku, '$(Map.new((:a(1),:b(2))))', 'a Map-typed container, two pairs';
}

# It round-trips.
is Map.new.raku.EVAL.raku, 'Map.new', 'the empty .raku evaluates back to an empty Map';
is Map.new.raku.EVAL.elems, 0, 'and it has no elements';

# The neighbouring empty collections keep their own shapes.
is Hash.new.raku, '{}', 'an empty Hash';
is Set.new.raku, 'set()', 'an empty Set';
is Bag.new.raku, 'bag()', 'an empty Bag';
is Mix.new.raku, 'mix()', 'an empty Mix';
