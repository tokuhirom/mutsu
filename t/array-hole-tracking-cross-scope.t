use Test;

# Autovivification gaps (holes) must be distinguished from existing elements by
# :exists / :k / :p / .keys, and this hole-tracking must travel with the array
# value across scope boundaries (closures, subs, methods). Previously the
# tracking lived in an env-name-keyed side table that was scoped to the
# assigning frame and lost on scope exit, so an outer array filled from inside a
# method/closure reported every gap as existing (HTTP::Status `method sink`).

plan 15;

# --- baseline: top-level sparse fill ---
{
    my @a;
    @a[100] = 'x';
    @a[200] = 'y';
    is-deeply @a[]:k, (100, 200), 'top-level sparse :k skips holes';
    is @a[50]:exists, False, 'top-level hole :exists is False';
    is @a[100]:exists, True, 'top-level filled :exists is True';
}

# --- fill inside a sub, read at top scope ---
{
    my @a;
    sub fill() { @a[100] = 'x'; @a[200] = 'y' }
    fill();
    is-deeply @a[]:k, (100, 200), 'sub-filled array :k skips holes (cross-scope)';
    is @a.elems, 201, 'sub-filled array elems counts up to last index';
}

# --- fill inside a method, read at top scope ---
{
    my @codes;
    class Filler {
        has $.code;
        method sink() { @codes[$!code] = self }
    }
    Filler.new(code => 10);
    Filler.new(code => 20);
    Filler.new(code => 30);
    42;  # sink the previous statements
    is-deeply @codes[]:k, (10, 20, 30), 'method-sink-filled array :k skips holes';
    is @codes[15]:exists, False, 'method-filled hole :exists is False';
    is-deeply (@codes[]:p).head, (10 => @codes[10]), 'method-filled :p head is first real pair';
}

# --- explicit Any assignment counts as existing ---
{
    my @a;
    @a[3] = Any;
    is @a[3]:exists, True, 'explicitly-assigned Any :exists is True';
    is-deeply @a[]:k, (3,), 'explicitly-assigned Any appears in :k';
}

# --- literal-constructed array with an Any element: all exist ---
{
    my @a = (1, Any, 3);
    is-deeply @a[]:k, (0, 1, 2), 'literal Any element still exists';
    is @a.elems, 3, 'literal array elems';
}

# --- delete creates a hole that :k skips ---
{
    my @a;
    @a[5] = 10;
    @a[2] = 20;
    @a[2]:delete;
    is-deeply @a[]:k, (5,), 'deleted index removed from :k';
}

# --- user .Str method drives eq / is comparison ---
{
    class Titled {
        has $.title;
        method Str() { $!title }
    }
    my $t = Titled.new(title => 'OK');
    ok $t eq 'OK', 'eq uses user Str method';
    is $t, 'OK', 'is() compares via user Str method';
}
