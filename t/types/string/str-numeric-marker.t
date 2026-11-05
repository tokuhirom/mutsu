use Test;

# X::Str::Numeric from a string→number coercion should carry the position
# marker, matching Rakudo:
#   Cannot convert string to number: trailing characters after number
#   in '123<HERE>abc' (indicated by <HERE>)
# `.Int` produced the marker but `.UInt`/`.Num`/`.Numeric` used hand-rolled
# messages that dropped it (and `.UInt` even reported the wrong reason).

plan 8;

# rakudo spells the marker `<HERE>` unless stderr is a terminal (then a coloured
# `⏏`); the test harness is never a terminal.
my $marker = "<HERE>";
my $expected = "Cannot convert string to number: trailing characters after number "
             ~ "in '123{$marker}abc' (indicated by {$marker})";

{
    my $msg; { "123abc".Int.sink;     CATCH { default { $msg = .message } } }
    is $msg, $expected, '.Int message has the position marker';
}
{
    my $msg; { "123abc".UInt.sink;    CATCH { default { $msg = .message } } }
    is $msg, $expected, '.UInt message has the position marker';
}
{
    my $msg; { "123abc".Num.sink;     CATCH { default { $msg = .message } } }
    is $msg, $expected, '.Num message has the position marker';
}
{
    my $msg; { "123abc".Numeric.sink; CATCH { default { $msg = .message } } }
    is $msg, $expected, '.Numeric message has the position marker';
}

# The exception is a real X::Str::Numeric (not a hand-rolled AdHoc).
throws-like { "123abc".Int.sink },     X::Str::Numeric, '.Int throws X::Str::Numeric';
throws-like { "123abc".Numeric.sink }, X::Str::Numeric, '.Numeric throws X::Str::Numeric';

# Valid strings still coerce.
is "1_000".Int, 1000, 'valid string still coerces with .Int';
is "3.14".Num, 3.14e0, 'valid string still coerces with .Num';
