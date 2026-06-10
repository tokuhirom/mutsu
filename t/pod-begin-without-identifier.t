use Test;

plan 5;

# `=begin` must be followed by an identifier naming the block.
throws-like "=begin\n", X::Syntax::Pod::BeginWithoutIdentifier;
throws-like "=begin   \n", X::Syntax::Pod::BeginWithoutIdentifier;

# The error propagates even when the bare `=begin` is not the first statement.
throws-like "say 1; =begin\nsay 2;", X::Syntax::Pod::BeginWithoutIdentifier;

# `=begin pod ... =end pod` is valid and produces no runtime output.
is EVAL("=begin pod\nHello\n=end pod\n42"), 42,
    'named =begin pod block parses';

# `=begin code` is also valid.
is EVAL("=begin code\nsome code\n=end code\n7"), 7,
    'named =begin code block parses';
