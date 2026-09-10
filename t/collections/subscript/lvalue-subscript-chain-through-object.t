use v6;
use Test;

# ADR-0067 slice 4: an lvalue subscript chain steps through an OBJECT by
# calling its AT-KEY/AT-POS in lvalue mode and descending into the container
# that comes back. Every row below was verified to produce byte-identical
# output under `raku` and `mutsu`.

plan 16;

class Q { has %.d is rw; method AT-KEY($k) is rw { %!d{$k} } }
class P { has @.d is rw; method AT-POS($i) is rw { @!d[$i] } }

# --- the acceptance rows -----------------------------------------------

# C3/H1: variable-rooted, depth 2. Was silently dropped (exit 0, no write).
{
    my $q = Q.new(d => {foo => [1, 2]});
    $q<foo>[0] = 99;
    is $q.d.gist, '{foo => [99 2]}', 'variable-rooted depth-2 chain writes through AT-KEY';
}

# C4: variable-rooted, depth 1 -- correct before, must stay correct.
{
    my $q = Q.new(d => {foo => [1, 2]});
    $q<foo> = 99;
    is $q.d.gist, '{foo => 99}', 'variable-rooted depth-1 assignment still writes through AT-KEY';
}

# H2: the explicit accessor spelling reaches the same location.
{
    my $q = Q.new(d => {foo => [1, 2]});
    $q.AT-KEY("foo")[0] = 99;
    is $q.d.gist, '{foo => [99 2]}', 'explicit .AT-KEY(...) call is an lvalue chain root';
}

# H5: depth 3. Used to REPLACE the object with a freshly autovivified Hash
# ("No such method 'd' for invocant of type 'Hash'").
{
    my $q = Q.new(d => {foo => {bar => [1, 2]}});
    $q<foo><bar>[0] = 99;
    is $q.d.gist, '{foo => {bar => [99 2]}}', 'depth-3 chain steps through the object instead of clobbering it';
}

# C2: the AT-POS twin.
{
    my $p = P.new(d => [[1, 2], [3, 4]]);
    $p[0][0] = 99;
    is $p.d.gist, '[[99 2] [3 4]]', 'positional chain writes through AT-POS';
}

# B1: method-rooted, depth 2 -- the ticket's headline. The compiler evaluates
# the accessor into a chain-root temp, so this is the variable-rooted walk with
# an object in the temp; it used to be refused loudly.
{
    my $u = Q.new(d => {q => Q.new(d => {foo => [1, 2]})});
    my $inner := $u<q>;
    $inner<foo>[0] = 99;
    is $u.d<q>.d.gist, '{foo => [99 2]}', 'a := alias of a subscriptable object is a chain root';
}

# --- autovivification through an object step ---------------------------

# A key the object does not have yet grows the container the next step asks for.
{
    my $q = Q.new(d => {foo => [1, 2]});
    $q<new>[0] = 9;
    is $q.d<new>.gist, '[9]', 'a missing key autovivifies the container the next subscript addresses';
    is $q.d<foo>.gist, '[1 2]', 'autovivification leaves the existing keys alone';
}

# The positional twin: an index past the end grows the object's array.
{
    my $p = P.new(d => [[1, 2], [3, 4]]);
    $p[2][0] = 9;
    is $p.d.gist, '[[1 2] [3 4] [9]]', 'an out-of-range AT-POS step grows through the returned location';
}

# A hash-valued element, so the step's container is a Hash rather than an Array.
{
    my $q = Q.new(d => {foo => {bar => 1}});
    $q<foo><bar> = 9;
    is $q.d.gist, '{foo => {bar => 9}}', 'the step descends into a Hash element too';
}

# --- an accessor that is NOT rw-capable --------------------------------

# It hands back no location, but the container it returns is the object's own
# (a method return shares its storage) -- raku mutates that object in place.
{
    my class R { has %.d; method AT-KEY($k) { %!d{$k} } }
    my $r = R.new(d => {foo => [1, 2]});
    $r<foo>[0] = 9;
    is $r.d.gist, '{foo => [9 2]}', 'a non-rw accessor still exposes the container it returns';
}

# --- regression rows ---------------------------------------------------

# H3: the := bind of exactly the same subscript. This is the producer the walk
# now consults, so it must keep working.
{
    my $q = Q.new(d => {foo => [1, 2]});
    my $e := $q<foo>;
    $e[0] = 99;
    is $q.d.gist, '{foo => [99 2]}', 'a := bind to an object element still aliases it';
}

# H4: a mutating method on the element, which never went through the walk.
{
    my $q = Q.new(d => {foo => [1, 2]});
    $q<foo>.push(99);
    is $q.d.gist, '{foo => [1 2 99]}', 'push through an object subscript still mutates in place';
}

# An inner object with ASSIGN-KEY keeps winning over the container write.
{
    my class Inner { has %.d is rw; method ASSIGN-KEY($k, $v) { %!d{$k} = "A:$v" } }
    my class Outer { has %.i is rw; method AT-KEY($k) is rw { %!i{$k} } }
    my $o = Outer.new(i => {x => Inner.new(d => {})});
    $o<x><y> = 5;
    is $o.i<x>.d.gist, '{y => A:5}', 'an inner ASSIGN-KEY object still takes the outermost write';
}

# An ordinary Hash root is untouched by any of this.
{
    my %h = a => [1, 2];
    %h<a>[0] = 99;
    is %h.gist, '{a => [99 2]}', 'a plain Hash root still walks the generic path';
}

# So is a plain nested autovivification.
{
    my %h;
    %h<a><b>[1] = 5;
    is %h.gist, '{a => {b => [(Any) 5]}}', 'plain deep autovivification is unchanged';
}

# vim: ft=raku
