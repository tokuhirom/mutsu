use Test;

# A named parameter whose rename key collides with an inner named alias
# (`:in(:$in)` -> external name "in" twice) is X::Signature::NameClash.

plan 4;

throws-like 'sub f(:in(:$in)) { }', X::Signature::NameClash, name => 'in';
throws-like 'sub g(:foo(:$foo)) { }', X::Signature::NameClash, name => 'foo';

# A rename with a differently-named inner alias is fine (two distinct keys).
sub h(:foo(:$bar)) { $bar }
is h(foo => 5), 5, ':foo(:$bar) binds via either key';

# A plain rename `:k($var)` with no inner colon is fine.
sub k(:label($text)) { $text }
is k(label => 'hi'), 'hi', ':label($text) rename works';
