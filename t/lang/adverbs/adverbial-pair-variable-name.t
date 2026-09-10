use Test;

# Extended identifiers: a variable's name may carry adverbial pair components.
# S02 / raku-doc Language/syntax.rakudoc: "The bracketing characters used ...
# do not count as part of it; only the quoted data matters", so <>, <<>>, «»,
# [] and () are interchangeable spellings of the same name.

plan 23;

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

# vim: expandtab shiftwidth=4
