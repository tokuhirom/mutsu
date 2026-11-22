# A `my subset` in a role body no longer hides the role after a pun

CSS::Properties' `t/box-fonts.t` died with "Could not find symbol
'CSS::Units'" once a font had been built. `CSS::Units` is a role group: its
plain candidate's `value` method parameterizes the parametric one, whose body
declares `my subset Length of CSS::Units`. A method called on a role's type
object runs in its pun, whose storage name is mangled (`CSS::Units\0pun`), and
the subset took that as its owner: its registry key began with `CSS::Units`
before the first NUL. Since `my subset`s became block-lexical (#18712), that
key is marked as a lexical declaration, and a lookup of `CSS::Units` read the
role itself as a lexical type of its module and hid it from the program.

A `my subset` is now qualified by its package's source name, as rakudo's
`.^name` reports it, so the role stays visible.
