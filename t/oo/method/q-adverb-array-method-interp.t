use v6;
use Test;

# The :a / :h quoting adverbs enable @-/%-sigil interpolation. A bare `@a`
# stays literal, but `@a[...]` (postcircumfix) AND `@a.method()` (method call)
# must interpolate. Previously mutsu required a postcircumfix and left a
# method call literal.

plan 8;

# The doc example (Language/quoting.rakudoc): method call on a Unicode array.
{
    my @þ = <33 44>;
    is Q:a "Array contains @þ.elems()", "Array contains 2",
        ':a interpolates a method call on the array';
}

{
    my @a = <x y z>;
    is Q:a "n=@a.elems()", "n=3", ':a method call interpolates';
    is Q:a "arr @a[]", "arr x y z", ':a postcircumfix still interpolates';
    is Q:a "bare @a end", "bare @a end", ':a leaves a bare array literal';
    is Q:a "dot @a. text", "dot @a. text", ':a leaves array + non-method dot literal';
}

# :h hash adverb, same rule.
{
    my %h = a => 1, b => 2;
    is Q:h "n=%h.elems()", "n=2", ':h interpolates a method call on the hash';
    is Q:h "v=%h<a>", "v=1", ':h postcircumfix still interpolates';
    is Q:h "bare %h end", "bare %h end", ':h leaves a bare hash literal';
}
