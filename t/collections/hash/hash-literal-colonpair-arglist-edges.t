use Test;

# Inside a `{ ... }` hash literal the colonpair parser has its own argument-list
# reader, and it demanded at least one expression between the parens. Two shapes
# that rakudo accepts therefore failed the WHOLE enclosing statement with
# "Confused. expected statement":
#
#   my $h = { :a() };            # an EMPTY argument list
#   my $h = { :a('x',) };        # a TRAILING comma
#
# Both come straight out of Crane's `t/patch.rakutest`
# (`{ :op<replace>, :path(), :value(1) }`), which could not be compiled at all.
# Outside a hash literal the general colonpair parser already accepted both.

plan 14;

# --- empty argument list -------------------------------------------------
{
    my $h = { :a() };
    is $h.raku, '${:a($( ))}', 'an empty colonpair arglist parses in a hash literal';
    is $h<a>.elems, 0, 'the value is the empty list';
}
{
    my $h = { :a(), :b(2) };
    is $h.raku, '${:a($( )), :b(2)}', 'an empty arglist mixes with ordinary pairs';
}
{
    my $h = { :a(), };
    is $h.raku, '${:a($( ))}', 'a trailing comma after the pair is fine too';
}

# --- trailing comma in the argument list ---------------------------------
{
    my $h = { :p('a',) };
    is $h.raku, '${:p($("a",))}', 'a trailing comma keeps a one-element list';
    is $h<p>.elems, 1, 'the one-element list has one element';
}
{
    my $h = { :p(1, 2,) };
    is $h.raku, '${:p($(1, 2))}', 'a trailing comma after several items';
    is $h<p>.elems, 2, 'the multi-element list is unchanged';
}

# --- both shapes together, as Crane spells them --------------------------
{
    my @patch = { :op<replace>, :path(), :value(1) },;
    is @patch.raku, '[{:op("replace"), :path($( )), :value(1)},]',
        'the empty-path patch element from Crane parses';
    is @patch.elems, 1, 'the trailing comma makes it a one-element array';
}
{
    my @patch = { :op<replace>, :path('a',), :value<Alpha> },;
    is @patch.raku, '[{:op("replace"), :path($("a",)), :value("Alpha")},]',
        'the one-element-path patch element from Crane parses';
}

# --- non-regression: the shapes that already worked ----------------------
{
    my $h = { :a(1) };
    is $h.raku, '${:a(1)}', 'a single argument is still a plain value, not a list';
}
{
    my $h = { :a(1, 2) };
    is $h.raku, '${:a($(1, 2))}', 'several arguments still make a list';
}
{
    my $h = { :a[], :b{} };
    is $h.raku, '${:a($[]), :b(${})}', 'the [] and {} colonpair forms are untouched';
}
