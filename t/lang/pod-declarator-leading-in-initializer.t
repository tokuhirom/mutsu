use Test;

plan 7;

# A leading `#|` written inside an initializer documents the declarator that
# follows it (roast S26-documentation/why-leading.t, block-leading.t).

my $anon-sub = #| Anonymous
    anon Str sub {};
is $anon-sub.WHY.Str, 'Anonymous', 'line doc after = documents the anon sub';

my $braced = #|{Braced}
    anon Int sub {};
is $braced.WHY.Str, 'Braced', 'block-form doc after = documents the anon sub';

my $block = #| this is a block
{;
};
is $block.WHY.Str, 'this is a block', 'line doc after = documents the block';

my $bound := #| bound
    anon sub {};
is $bound.WHY.Str, 'bound', 'line doc after := documents the anon sub';

#| Enumeration
enum Colors < Red Green Blue >;
is Colors.WHY.Str, 'Enumeration', 'a later declaration keeps its own doc';

my $plain = anon sub {};
nok $plain.WHY.defined, 'an undocumented anon sub has no doc';

my $s = "a = #| not a doc";
is $s, 'a = #| not a doc', 'a #| inside a string is untouched';
