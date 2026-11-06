use Test;
use lib 't/lib';

# A dispatcher handed out by `sub EXPORT` lists its own candidates next to
# every other candidate of the name, whatever order the imports come in
# (#16695): `&trait_mod:<is>.candidates` is the union.
plan 3;

my $code = '&trait_mod:<is>.candidates.map(*.signature.raku).grep(/xdtrait/).elems';

is EVAL("use Test; use ExportSubTraitDispatcher; $code"), 1,
    'use Test, then the hook module';
is EVAL("use ExportSubTraitDispatcher; use Test; $code"), 1,
    'the hook module, then use Test';
is EVAL("use ExportSubTraitDispatcher; $code"), 1, 'the hook module alone';
