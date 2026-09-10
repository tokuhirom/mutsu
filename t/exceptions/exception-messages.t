use Test;

plan 15;

# X::Method::NotFound
{
    try { die X::Method::NotFound.new(method => "foo", typename => "Bar") };
    is $!.message, "No such method 'foo' for invocant of type 'Bar'",
        "X::Method::NotFound .message";
}

# X::Str::Numeric
{
    try { die X::Str::Numeric.new(source => "foo", reason => "not numeric", pos => 0) };
    ok $!.message.contains("Cannot convert string to number"),
        "X::Str::Numeric .message contains expected prefix";
}

# X::Undeclared
{
    try { die X::Undeclared.new(symbol => '$foo', what => "Variable") };
    is $!.message, "Variable '\$foo' is not declared",
        "X::Undeclared .message";
}

# X::Cannot::Lazy
{
    try { die X::Cannot::Lazy.new(action => "sort") };
    is $!.message, "Cannot sort a lazy list",
        "X::Cannot::Lazy .message";
}

# X::ControlFlow::Return
{
    try { die X::ControlFlow::Return.new };
    is $!.message, "Attempt to return outside of any Routine",
        "X::ControlFlow::Return .message";
}

# X::OutOfRange
{
    try { die X::OutOfRange.new(got => 5, range => "0..3", what => "Index") };
    is $!.message, "Index out of range. Is: 5, should be in 0..3",
        "X::OutOfRange .message";
}

# X::Immutable (with method)
{
    try { die X::Immutable.new(typename => "List", method => "push") };
    is $!.message, "Cannot call 'push' on an immutable 'List'",
        "X::Immutable .message with method";
}

# X::Immutable (without method)
{
    try { die X::Immutable.new(typename => "List") };
    is $!.message, "Cannot modify an immutable List",
        "X::Immutable .message without method";
}

# X::Redeclaration
{
    try { die X::Redeclaration.new(symbol => "foo") };
    is $!.message, "Redeclaration of symbol 'foo'.",
        "X::Redeclaration .message";
}

# X::StubCode
{
    try { die X::StubCode.new };
    is $!.message, "Stub code executed",
        "X::StubCode .message";
}

# X::Bind
{
    try { die X::Bind.new };
    is $!.message, "Cannot use bind operator with this left-hand side",
        "X::Bind .message";
}

# X::Bind (with target)
{
    try { die X::Bind.new(target => "my thing") };
    is $!.message, "Cannot bind to my thing",
        "X::Bind .message with target";
}

# .gist includes backtrace info
{
    try { die X::StubCode.new };
    ok $!.gist.contains("Stub code executed"),
        "X::StubCode .gist contains the message";
}

# X::Multi::NoMatch
{
    try { die X::Multi::NoMatch.new(name => "foo") };
    is $!.message, "Cannot resolve caller foo; none of these signatures match",
        "X::Multi::NoMatch .message";
}

# X::Multi::Ambiguous
{
    try { die X::Multi::Ambiguous.new(name => "foo") };
    is $!.message, "Ambiguous call to 'foo'; these signatures all match",
        "X::Multi::Ambiguous .message";
}
