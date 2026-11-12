use lib 't/lib';
use Test;

# A module's `my class` read as a bareword from the module's own subs resolves
# to the lexical type every time (the site memo now remembers the lexical
# storage name, #17337), and a type capture of the same spelling still wins
# over the remembered answer.

plan 5;

use BarewordLexicalClass;

is hidden-type().tag, 'hidden', 'the bareword names the module-lexical class';
is (^50).map({ hidden-type().tag }).unique.join(','), 'hidden', 'and keeps doing so on every call';
is hidden-or-capture(42).^name, 'Int', 'a type capture of the same spelling wins';
is hidden-type().tag, 'hidden', 'and does not poison the bareword site';
is await(start { hidden-type().tag }), 'hidden', 'a thread resolves it too';
