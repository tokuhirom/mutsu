use Test;

# A qualified name with a trailing `::` denotes the package Stash
# (`::EXPORT::DEFAULT::`). The class-literal parser stopped at the trailing `::`,
# leaving a stray `::` that broke the enclosing expression when the name appeared
# inside parentheses / a list — e.g. Test::Class's
#   sub EXPORT { (|::EXPORT::DEFAULT::, |Test::EXPORT::DEFAULT::).Map }
# failed to parse. The trailing `::` is now consumed so the name is a single term.

plan 5;

# The exact Test::Class re-export idiom must parse.
lives-ok { EVAL 'sub EXPORT { (|::EXPORT::DEFAULT::, |Test::EXPORT::DEFAULT::).Map }' },
    'trailing-:: package names in a parenthesized slip list parse';

# A single trailing-:: package inside parens.
lives-ok { EVAL 'my $x = (::EXPORT::DEFAULT::)' },
    'a single trailing-:: package parses inside parens';

# Followed by a postfix method call.
lives-ok { EVAL 'my $x = (::EXPORT::DEFAULT::).Map' },
    'trailing-:: package followed by a method call parses';

# No regression: a qualified name without a trailing `::` still parses.
lives-ok { EVAL 'my $x = (Test::EXPORT::DEFAULT)' },
    'a qualified name without a trailing :: still parses';

# No regression: an indirect lookup still parses.
lives-ok { EVAL 'my $name = "Int"; my $x = (::($name))' },
    'an indirect name lookup still parses inside parens';
