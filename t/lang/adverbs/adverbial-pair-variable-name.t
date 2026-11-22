use Test;

# Extended identifiers: a variable's name may carry adverbial pair components.
# S02 / raku-doc Language/syntax.rakudoc: "The bracketing characters used ...
# do not count as part of it; only the quoted data matters", so <>, <<>>, «»,
# [] and () are interchangeable spellings of the same name.

plan 35;

# --- keyed colon pairs: $foo:bar<baz> ------------------------------------

{
    my $foo:bar<baz> = 'quux';
    is $foo:bar<baz>,     'quux', 'keyed adverb, <> declaration read back with <>';
    is $foo:bar«baz»,     'quux', 'keyed adverb, <> declaration read back with «»';
    is $foo:bar['baz'],   'quux', 'keyed adverb, <> declaration read back with []';
    is $foo:bar('baz'),   'quux', 'keyed adverb, <> declaration read back with ()';
    is $foo:bar<<baz>>,   'quux', 'keyed adverb, <> declaration read back with <<>>';
}

{
    # ... and the same name declared through each of the other spellings.
    my $a:bar«baz» = 'one';
    is $a:bar<baz>, 'one', '«» declaration canonicalizes to the <> name';

    my $b:bar['baz'] = 'two';
    is $b:bar<baz>, 'two', '[] declaration canonicalizes to the <> name';

    my $c:bar('baz') = 'three';
    is $c:bar<baz>, 'three', '() declaration canonicalizes to the <> name';

    my $d:bar<<baz>> = 'four';
    is $d:bar<baz>, 'four', '<<>> declaration canonicalizes to the <> name';
}

# --- key-less colon pairs: $take-me:<home> -------------------------------
# The value alone spells the pair. Note `(...)` is NOT valid here: `:(...)`
# is a signature literal, and raku rejects `my $t:("home")`.

{
    my $take-me:<home> = 'Where the glory has no end';
    is $take-me:<home>,   'Where the glory has no end', 'key-less adverb read with <>';
    is $take-me:['home'], 'Where the glory has no end', 'key-less adverb read with []';
    is $take-me:«home»,   'Where the glory has no end', 'key-less adverb read with «»';
    is $take-me:<<home>>, 'Where the glory has no end', 'key-less adverb read with <<>>';
}

{
    my $e:«home» = 'guillemet decl';
    is $e:<home>, 'guillemet decl', 'key-less «» declaration canonicalizes to <>';

    my $f:['home'] = 'bracket decl';
    is $f:<home>, 'bracket decl', 'key-less [] declaration canonicalizes to <>';
}

# --- stacking, ordering, other sigils ------------------------------------

{
    my $g:<a>:<b> = 'stacked';
    is $g:<a>:<b>, 'stacked', 'two key-less adverbs stack';

    my $h:foo<a>:<b> = 'mixed';
    is $h:foo<a>:<b>, 'mixed', 'keyed and key-less adverbs mix';

    # Order of colon pairs is significant (syntax.rakudoc).
    my $i:b<c>:d<e> = 100;
    my $i:d<e>:b<c> = 200;
    is $i:b<c>:d<e>, 100, 'adverb order is significant (1)';
    is $i:d<e>:b<c>, 200, 'adverb order is significant (2)';
}

{
    my @arr:<a> = 1, 2;
    is @arr:<a>.join(','), '1,2', 'key-less adverb on an @ variable';

    my %hsh:<a> = x => 1;
    is %hsh:<a><x>, 1, 'key-less adverb on a hash variable';
}

# --- multi-word values and adverbs without a value -----------------------

{
    my $j:foo<a b> = 'words';
    is $j:foo«a b», 'words', 'multi-word adverb value canonicalizes across <> and «»';

    my $k:foo<a>:bar:baz<c> = 'novalue';
    is $k:foo<a>:bar:baz<c>, 'novalue', 'a valueless adverb between valued ones';
}

# --- subscript-adverb names are ordinary keys in a variable name ---------
# rakudo reads `$y:k[1,2]` as the variable `$y:k<1 2>`; `:k`, `:v`, `:kv`,
# `:p`, `:exists` and `:delete` are subscript adverbs only after a subscript.

{
    my $y:k[1,2] = 3;
    is $y:k<1 2>, 3, ':k with a [] list value names k<1 2>';
    is $y:k[1,2], 3, '... read back with the same [] spelling';
    is $y:k(1,2), 3, '... and with ()';

    my $x:k = 5;
    my $x = 7;
    is $x:k, 5, 'a valueless :k is part of the name';
    is $x, 7, '... distinct from the bare variable';

    my @a:v<1> = 1, 2;
    is @a:v<1>.elems, 2, ':v on an array name';

    my $p:exists = 'e';
    is $p:exists, 'e', ':exists on a scalar name';
}

throws-like '$q:delete', Exception, 'an undeclared $q:delete is not $q with an adverb';

# A positional capture variable is not a long name: `$0:exists` is
# `$/[0]:exists`, a subscript adverb.
{
    'ab' ~~ /(a)/;
    is $0:exists, True, '$0:exists is the subscript adverb';
    is $1:exists, False, '... and false for a missing capture';
    is $0:k, 0, '$0:k is the index';
    is "$0:exists", 'a:exists', 'interpolated $0 leaves the adverb as text';
}

# vim: expandtab shiftwidth=4
