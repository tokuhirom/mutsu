use v6;
use Test;

# `imported-listop <a b c>` only parses as a call if the parser already knows
# the imported name is a routine. When it does not, `<` is taken as the infix
# less-than and the quote-word list is a hard parse error -- while the
# parenthesised `imported-listop(<a b c>)` form works fine, which is the tell.
#
# mutsu's parser pre-scans every `use`d module for the names it exports to
# answer exactly that question. The scan used to keep only the subs whose
# `is export` trait carried the DEFAULT or MANDATORY tag, so a sub exported
# under a custom tag was invisible to it -- even for an importer that asked
# for that tag, because the scan is handed the module name and never the
# importer's tag list.
#
# `tag-export-parse-time-only.t` pins the other half: widening the scan is
# parse-time knowledge only, and must not make a withheld name callable.
#
# https://github.com/tokuhirom/mutsu/issues/7939

plan 12;

use lib 't/lib';
use ImportedListopAngle :extra, :DEFAULT;

# `root` is exported untagged (DEFAULT). This half is the shape the issue
# reported: an ordinary `is export` listop with a quote-word argument.
is root(<abcd abce abde>), 'ab',
    'the parenthesised call form works';

is (root <abcd abce abde>), 'ab',
    'a listop call with a quote-word argument parses';

is (root <abcd abcd abcd>), 'abcd',
    'a quote-word list of identical words parses';

is (root <foo bar baz>), '',
    'a quote-word list with no common prefix parses';

is (root <foo>), 'foo',
    'a single-element quote-word list parses';

is (root <<abcd abce>>), 'abc',
    'a double-angle quote-word argument parses';

is (root qw{abcd abce}), 'abc',
    'a qw{} argument parses';

is (root <abcd abce>).chars, 3,
    'a postfix applies to the listop call result, not to the argument';

is (tally <a b c>), 3,
    'a second untagged export parses as a listop too';

# `joined` is exported only under `:extra`. Before the fix this line was a
# parse error, not a wrong answer.
is (joined <a b c>), 'a-b-c',
    'a tag-exported listop with a quote-word argument parses';

# The listop reading must not swallow a genuine comparison.
ok (1 < 2), 'infix < still parses as less-than';
ok (1 < 2 < 3), 'a chained infix < still parses';
