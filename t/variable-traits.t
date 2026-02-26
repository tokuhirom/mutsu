use Test;

plan 4;

throws-like q[my $a is readonly = 5;], X::Comp::Trait::Unknown,
    q[unsupported variable trait "is readonly" throws X::Comp::Trait::Unknown];

throws-like q[(my $a is readonly) = 5;], X::Comp::Trait::Unknown,
    q[parenthesized declaration also rejects unsupported variable trait];

lives-ok q[my $a is default(41) = 42;], q[supported variable trait parses];

throws-like q[my $a is definitely-invalid = 5;], X::Comp::Trait::Unknown,
    q[unknown variable trait is rejected];
