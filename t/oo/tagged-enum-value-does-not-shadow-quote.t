use lib 't/lib';
use Test;

# An enum value spelled like a quote language (`s`, `m`, `q`, ...) shadows that
# quote construct once it is in scope -- rakudo reads `s/a/b/` as a division by
# `s` then. So the parse must learn an imported enum value only when the `use`
# really imports it. CSS::Units declares `my enum Time is export(:Time)
# « :s(1.0) :ms(0.001) »`, and before this fix a plain `use CSS::Units;` (or a
# module that itself imported `:Time`) turned every later `s///` into a parse
# error (ecosystem dist CSS::TagSet).

use TaggedEnumQuote::Units;
use TaggedEnumQuote::Mid;
# An adverb glued to the module name is a name adverb, not an import tag:
# rakudo imports DEFAULT here and does not import `:Time`.
use TaggedEnumQuote::Units:Time;

plan 7;

my $k = 'smallcaps';
$k ~~ s/smallcaps/small-caps/;
is $k, 'small-caps', 's/// still parses after a use that does not import the :Time tag';

is TaggedEnumQuote::Mid::mid(), 's', 'the module that imported :Time sees its own enum value';

my $m = 'abc';
$m ~~ s/b/B/;
is $m, 'aBc', "a module's own imports are not re-exported to its importer";

{
    use TaggedEnumQuote::Units :Time;
    is s.value, 1.0, 'a use naming the tag does import the enum value';
}

{
    use TaggedEnumQuote::Units :&dimension, :pt;
    is dimension(1), 'dim-1', ':&name names the tag `name`';
    is 5pt, '5pt', 'tags after a :&name adverb are still imported';
}

nok (try EVAL 'dimension(2)'), 'the glued :Time adverb imported nothing tagged';
