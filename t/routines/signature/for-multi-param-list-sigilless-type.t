use v6;
use Test;

plan 4;

# A scalar in an explicit list keeps its container. A sigilless multi-param
# loop binding must therefore preserve the scalar's type constraint too.
my subset Small of Int where 0 <= $_ <= 1;
my Small $source;
my $error;
my $got;

for $source, 1000 -> \slot, $value {
    CATCH {
        $error = .^name;
        $got = .got;
        is .Str, 'Type check failed in assignment to $source; expected Small but got Int (1000)',
            'the aliased list item reports the source constraint';
        next;
    }
    slot = $value;
}

is $error, 'X::TypeCheck::Assignment', 'the aliased write raises an assignment error';
is $got, 1000, 'the error carries the rejected value';
is $source, Small, 'the failed write leaves the typed source unchanged';
