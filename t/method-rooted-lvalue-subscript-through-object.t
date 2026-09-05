use v6;
use Test;

# ADR-0067 slice 5: the method-rooted half of "an lvalue subscript chain steps
# through an object". Slice 4 taught the variable-rooted walkers the step; this
# file pins the spellings whose root is an accessor call. Every row produces
# byte-identical output under `raku` and `mutsu`.

plan 12;

class Q { has %.d is rw; method AT-KEY($k) is rw { %!d{$k} } }
class P { has @.d is rw; method AT-POS($i) is rw { @!d[$i] } }

# --- depth 1: the accessor's object serves the subscript itself --------

# B4 -- the row that had no walker at all: `__mutsu_index_assign_method_lvalue`
# found no ASSIGN-KEY and fell into plain-container handling, dropping the write.
{
    my class U { has Q $.query = Q.new(d => {foo => [1, 2]}) }
    my $u = U.new;
    $u.query<foo> = 99;
    is $u.query.d.gist, '{foo => 99}', 'method-rooted depth-1 assignment reaches AT-KEY';
}

# B9 -- the same, autovivifying a key the object does not have yet.
{
    my class U { has Q $.query = Q.new(d => {foo => [1, 2]}) }
    my $u = U.new;
    $u.query<new> = 7;
    is $u.query.d<new>, 7, 'method-rooted depth-1 assignment autovivifies a missing key';
    is $u.query.d<foo>.gist, '[1 2]', 'and leaves the existing key alone';
}

# B10 -- the AT-POS twin.
{
    my class V { has P $.slot = P.new(d => [[1, 2], [3, 4]]) }
    my $v = V.new;
    $v.slot[0] = 99;
    is $v.slot.d.gist, '[99 [3 4]]', 'method-rooted depth-1 assignment reaches AT-POS';
}

# --- deeper method-rooted chains ---------------------------------------

# B1 -- the ticket's headline.
{
    my class U { has Q $.query = Q.new(d => {foo => [1, 2]}) }
    my $u = U.new;
    $u.query<foo>[0] = 99;
    is $u.query.d.gist, '{foo => [99 2]}', 'method-rooted depth-2 chain writes through AT-KEY';
}

# B8 -- depth 3. The chain-root temp holds the object, which used to be refused
# outright ("it returned Q, not an Array or Hash container").
{
    my class U { has Q $.query = Q.new(d => {foo => {bar => [1, 2]}}) }
    my $u = U.new;
    $u.query<foo><bar>[0] = 99;
    is $u.query.d.gist, '{foo => {bar => [99 2]}}', 'method-rooted depth-3 chain steps through the object';
}

# B6 -- the `:=`-bound alias spelling of the same root.
{
    my class U { has Q $.query = Q.new(d => {foo => [1, 2]}) }
    my $u = U.new;
    my $t := $u.query;
    $t<foo>[0] = 99;
    is $u.query.d.gist, '{foo => [99 2]}', 'a := alias of an accessor-returned object is a chain root';
}

# --- regression rows ---------------------------------------------------

# An object supplying ASSIGN-KEY still takes the write through it, not through
# AT-KEY: the setter is dispatched and can transform the value.
{
    my class Inner { has %.d is rw; method AT-KEY($k) is rw { %!d{$k} }; method ASSIGN-KEY($k, $v) { %!d{$k} = "A:$v" } }
    my class W { has Inner $.i = Inner.new(d => {}) }
    my $w = W.new;
    $w.i<y> = 5;
    is $w.i.d.gist, '{y => A:5}', 'ASSIGN-KEY still wins over the AT-KEY location';
}

# Ordinary container attributes are untouched by any of this.
{
    my class H { has %.h is rw }
    my $o = H.new(h => {a => 1});
    $o.h<a> = 9;
    $o.h<b> = 2;
    is $o.h.gist, '{a => 9, b => 2}', 'a plain hash attribute still assigns and autovivifies';
}

{
    my class A { has @.a is rw }
    my $o = A.new(a => [1, 2]);
    $o.a[0] = 9;
    is $o.a.gist, '[9 2]', 'a plain array attribute still assigns';
}

{
    my class A { has @.a is rw }
    my $o = A.new(a => [[1, 2], [3, 4]]);
    $o.a[1][0] = 9;
    is $o.a.gist, '[[1 2] [9 4]]', 'a plain array attribute still walks a depth-2 chain';
}

# A root that genuinely is not a location keeps refusing loudly rather than
# silently dropping the write.
{
    my class W { method thing { 42 } }
    my $w = W.new;
    dies-ok { $w.thing<a>[0] = 1 }, 'a non-container chain root is still refused';
}

# vim: ft=raku
