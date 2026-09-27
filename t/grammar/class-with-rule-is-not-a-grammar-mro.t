use Test;

# A `class`/`role` that merely *declares* a `token`/`rule` is not thereby a
# grammar. mutsu used to detect grammar-ness for `.^mro`/`.^parents` by
# scanning for any registered token/rule definition under the package's
# name, so a plain class (or a role pun'd as an `is` parent) that declares a
# `rule`/`token` wrongly threaded Grammar -> Match -> Capture -> Cool into
# its MRO.
# https://github.com/tokuhirom/mutsu/issues/9655

plan 3;

class N { rule foo { x } }
is N.^parents.join(' '), '', 'a plain class with a rule has no parents';
is N.^mro.map(*.^name).join(' '), 'N Any Mu',
    'a plain class with a rule does not thread the Grammar chain';

role BaseG { token number { \d+ } }
class C is BaseG { }
is C.^mro.map(*.^name).join(' '), 'C BaseG Any Mu',
    'a class inheriting a role that declares a token does not thread the Grammar chain';
