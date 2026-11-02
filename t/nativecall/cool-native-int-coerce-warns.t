use Test;

# #15892: a native integer coercion of the undefined `Cool` raises the resumable
# "uninitialized value in numeric context" warning and resumes with zero.

my @names = <int int8 int16 int32 int64 uint uint8 uint16 uint32 uint64 byte>;
plan 2 * @names + 4;

for @names -> $m {
    my $warnings = 0;
    my $n;
    {
        CONTROL { when CX::Warn { $warnings++; .resume } }
        $n = Cool."$m"();
    }
    is $n, 0, "Cool.$m resumes with zero";
    is $warnings, 1, "Cool.$m raises one CX::Warn";
}

is 5.int8, 5, 'a concrete Int is unchanged';
is "7".uint8, 7, 'a concrete Str is unchanged';
is 300.uint8, 44, 'an out-of-range value still wraps';
throws-like { Int.int8 }, Exception, 'an Int type object is still refused';
