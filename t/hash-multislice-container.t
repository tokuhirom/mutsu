use v6.e.PREVIEW;
use Test;

plan 25;

# --- Test-function references (&is-deeply etc.) -------------------------
{
    my &fn = &is-deeply;
    ok &fn.defined, 'Test function &-reference is defined';
    &fn((1, 2).sort, (1, 2), 'calling a Test function through an &-var works');
    (True ?? &pass !! &flunk)('conditional Test-function reference dispatches');
}

# --- Whatever / list dims in hash multislice adverbs --------------------
{
    my %hash = a => { b => { c => 42, d => 666, e => { f => 314 } } };
    is-deeply (%hash{*;"b";"c"}:exists), (True,), ':exists with Whatever dim';
    is-deeply (%hash{*;"b";"x"}:exists), (False,), ':exists Whatever + missing leaf';
    is-deeply (%hash{*;"b";"c"}:k), (("a","b","c"),), ':k rebuilds string key tuple';
    is-deeply (%hash{*;"b";"c"}:kv), (("a","b","c"), 42), ':kv with Whatever dim';
    is-deeply (%hash{*;"b";"c"}:p), (Pair.new(("a","b","c"), 42),), ':p with Whatever dim';
    is-deeply (%hash{*;"b";"c"}:v), (42,), ':v with Whatever dim';
    is-deeply (%hash{"a";"b";("c","d")}:v), (42, 666), ':v with key-list dim';
    is-deeply (%hash{"a";"b";("c","d")}:k),
      (("a","b","c"), ("a","b","d")), ':k with key-list dim';

    is-deeply (%hash{*;"b";"c"}:delete), (42,), ':delete with Whatever dim';
    is-deeply %hash, %(a => { b => { d => 666, e => { f => 314 } } }),
      ':delete with Whatever dim removed the right leaf';
}

# --- \target binds a hash multislice lvalue ------------------------------
sub assign-it(\target, \values) { target = values }

{
    my %h = a => { b => { c => 42 } };
    assign-it(%h{"a";"b";"c"}, 999);
    is %h<a><b><c>, 999, 'sigilless bind writes an existing hash multislice leaf';
}

{
    my %h = a => { b => { c => 42 } };
    assign-it(%h{"a";"b";"x"}, 999);
    is %h<a><b><x>, 999, 'sigilless bind vivifies a missing terminal key';
    is %h<a><b><c>, 42, 'sibling key untouched by vivification';
}

{
    my %h = a => { b => { c => 42 } };
    assign-it(%h{"x";"y";"z"}, 7);
    is %h<x><y><z>, 7, 'sigilless bind walk-creates a fully missing path';
}

{
    my %h = a => { b => { c => 1, d => 2 } };
    assign-it(%h{"a";"b";("c","d")}, (10, 20));
    is %h<a><b><c>, 10, 'list-dim bind distributes element-wise (1)';
    is %h<a><b><d>, 20, 'list-dim bind distributes element-wise (2)';
}

# expression-context assignment through the bind (the roast assignable-ok shape)
{
    my %h = a => { b => { c => 42 } };
    sub expr-assign(\target, \values) { my $r = (target = values); $r }
    is expr-assign(%h{"a";"b";"x"}, 5), 5, 'expression-context assign returns the value';
    is %h<a><b><x>, 5, 'expression-context assign reaches the hash';
}

# assignment inside a closure that captured the bound target
{
    my %h = a => { b => { c => 42 } };
    sub closure-assign(\target, \values) {
        my &blk = { target = values; };
        blk();
    }
    closure-assign(%h{"a";"b";"x"}, 999);
    is %h<a><b><x>, 999, 'closure-context assign vivifies through the capture';
    closure-assign(%h{"a";"b";"c"}, 777);
    is %h<a><b><c>, 777, 'closure-context assign writes an existing leaf';
}

# --- whole-container reassignment keeps container identity ---------------
{
    my %h = a => 1;
    my @list = "x", %h;
    sub restock(--> Nil) { %h = a => 2; }
    restock();
    is-deeply @list[1]<a>, 2,
      'cross-frame whole-hash reassign updates a by-value list capture';
}

{
    my @a = 1, 2;
    my @list = "x", @a;
    sub refill(--> Nil) { @a = 3, 4; }
    refill();
    is-deeply @list[1][0], 3,
      'cross-frame whole-array reassign updates a by-value list capture';
}

done-testing;
