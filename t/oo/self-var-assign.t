use v6;
use Test;

# `my $self` is an ordinary variable — scalars are stored sigil-less in
# mutsu, so it must not collide with the compile-time "cannot assign to the
# invocant self" check, which applies only inside a method body.
# (Text::CSV's 85_util.t: `my $self; { $self = $csv.header($fh); CATCH ... }`)

plan 4;

my $self;
$self = 42;
is $self, 42, 'assignment to a my $self variable works at top level';

{
    $self = "in-block";
}
is $self, "in-block", 'assignment inside a nested block too';

sub takes-self { my $self = "sub"; $self = "reassigned"; $self }
is takes-self(), "reassigned", 'and inside a sub body';

# The invocant keyword stays immutable inside a method (roast
# S12-class/basic.t pins the exception).
class WritableSelf { method f { self = 5 } }
dies-ok { WritableSelf.new.f }, 'self = ... inside a method still dies';

done-testing;
