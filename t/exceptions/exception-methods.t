use Test;

plan 8;

# Basic .message method
{
    try { die "oops" };
    is $!.message, "oops", '$!.message returns the error message';
}

# .line method returns an Int
{
    try { die "test" };
    ok $!.line.defined, '$!.line is defined';
    isa-ok $!.line, Int, '$!.line returns an Int';
}

# .file method returns a Str
{
    try { die "test" };
    ok $!.file.defined, '$!.file is defined';
    isa-ok $!.file, Str, '$!.file returns a Str';
}

# .backtrace method returns a string
{
    try { die "test" };
    ok $!.backtrace.defined, '$!.backtrace is defined';
}

# A successful try leaves Nil in $! (rakudo's RakuAST frontend; the legacy one
# left the Any type object), so Exception methods called on it answer Nil.
{
    try { 1 + 1 };
    is $!.^name, 'Nil', '$! is Nil when there was no error';
    is $!.message, Nil, '$!.message is Nil when there was no error';
}

done-testing;
