use Test;

plan 8;

# X::TypeCheck::Assignment - thrown on typed variable assignment failure
throws-like { my Int $x = "hello" }, X::TypeCheck::Assignment,
    "typed variable assignment throws X::TypeCheck::Assignment";

# X::TypeCheck::Assignment for array elements
throws-like { my Int @a; @a[0] = "hi" }, X::TypeCheck::Assignment,
    "typed array element assignment throws X::TypeCheck::Assignment";

# X::TypeCheck::Argument - thrown when calling a sub with wrong argument types
throws-like { sub f(Int $x) {}; f("hi") }, X::TypeCheck::Argument,
    "calling sub with wrong type throws X::TypeCheck::Argument";

# X::TypeCheck (parent type) should match X::TypeCheck::Argument
throws-like { sub f(Int $x) {}; f("hi") }, X::TypeCheck,
    "parent type X::TypeCheck matches Argument subtype";

# X::Assignment::RO - thrown when assigning to a readonly value
throws-like { sub f($x) { $x = 5 }; f(42) }, X::Assignment::RO,
    "assignment to readonly parameter throws X::Assignment::RO";

# X::TypeCheck should match X::TypeCheck::Assignment (parent type)
throws-like { my Int $x = "hello" }, X::TypeCheck,
    "parent type X::TypeCheck matches Assignment subtype";

# Named attribute matchers
throws-like { my Int $x = "hello" }, X::TypeCheck::Assignment,
    message => /expected.*Int/,
    "attribute matcher on exception message works";

# Multiple typed parameters - first mismatch fails
throws-like { sub g(Str $a, Int $b) {}; g(42, "hi") }, X::TypeCheck::Argument,
    "multiple params: wrong arg type throws X::TypeCheck::Argument";
