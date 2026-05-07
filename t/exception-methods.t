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

# When $! is Nil (no error), methods return Nil
{
    try { 1 + 1 };
    ok !$!.message.defined, '$!.message is Nil when no error';
    ok !$!.line.defined, '$!.line is Nil when no error';
}

done-testing;
