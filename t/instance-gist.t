use v6;
use Test;

# The default `.gist` of a DEFINED user-class instance is `ClassName.new(attr
# => value, ...)` (same as `.raku`), matching raku — not the type-object form
# `ClassName()`. `say $obj` uses gist, so this is what `say` of an attributeful
# instance prints. A user-defined `gist` method still takes precedence, and a
# type OBJECT still gists as `(ClassName)`.

plan 11;

class Point { has $.x; has $.y }
class Empty { }
class Named { has $.s }

# --- defined instances gist as ClassName.new(...) ---
is Point.new(:x(3), :y(4)).gist, 'Point.new(x => 3, y => 4)',
    'instance with two attributes gists as .new(...)';
is Empty.new.gist, 'Empty.new', 'attributeless instance gists as .new';
is Named.new(:s("hi")).gist, 'Named.new(s => "hi")',
    'string attribute is quoted in instance gist (like .raku)';

# --- gist == raku for a default instance ---
{
    my $p = Point.new(:x(1), :y(2));
    is $p.gist, $p.raku, 'default instance gist matches its raku';
}

# --- say uses gist ---
{
    my $out = Point.new(:x(5), :y(6)).gist;
    is $out, 'Point.new(x => 5, y => 6)', 'gist used by say is the .new(...) form';
}

# --- nested instances render recursively ---
class Outer { has $.inner }
is Outer.new(:inner(Point.new(:x(7), :y(8)))).gist,
    'Outer.new(inner => Point.new(x => 7, y => 8))',
    'nested instance gists recursively';

# --- a type OBJECT still gists as (ClassName) ---
is Point.gist, '(Point)', 'type object gists as (ClassName), not .new';
{
    my Point $u;
    is $u.gist, '(Point)', 'undefined typed variable gists as the type object';
}

# --- a user-defined gist method still wins ---
class Custom { has $.z; method gist { "custom<{$!z}>" } }
is Custom.new(:z(9)).gist, 'custom<9>', 'user-defined gist takes precedence';

# --- .= on a self-returning method round-trips through gist ---
class Counter { has $.n is rw; method bump { $!n = ($!n // 0) + 1; self } }
{
    my Counter $c .= new;
    $c .= bump;
    is $c.n, 1, '.= on a self-returning method mutates and stores back';
}

# --- a callable (Method) instance keeps its own gist, not `Method.new(...)` ---
{
    class WithMethod { method foo { } }
    nok WithMethod.^lookup('foo').gist.contains('Method.new'),
        'a Method instance does not gist as Method.new(...)';
}
