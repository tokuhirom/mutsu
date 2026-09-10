use Test;

# `=begin` with no block name is X::Syntax::Pod::BeginWithoutIdentifier. That
# already held when the `=begin` was followed by more source, but mutsu's EVAL
# trims its argument -- so `EVAL "=begin\n"` arrived at the parser as a bare
# `=begin` at end of input, which fell out as a generic parse failure instead.

plan 5;

throws-like "=begin\n", X::Syntax::Pod::BeginWithoutIdentifier,
    'a =begin with no identifier';

throws-like "=begin   \n", X::Syntax::Pod::BeginWithoutIdentifier,
    'trailing spaces are not an identifier';

throws-like "=begin\n=end\n", X::Syntax::Pod::BeginWithoutIdentifier,
    'nor is a following =end';

throws-like "say 1;\n=begin\nsay 2;", X::Syntax::Pod::BeginWithoutIdentifier,
    'it is reported when it is not the first statement';

# A named block is unaffected.
is EVAL("=begin pod\nHello\n=end pod\n42"), 42,
    'a named =begin pod block still parses';
