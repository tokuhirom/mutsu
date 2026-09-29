use Test;

plan 4;

sub apply-one(&g:($)) { g(2) }
my $seen = 0;
apply-one(-> $n { $seen = $n });
is $seen, 2, 'a pointy block supplies its positional signature to a Callable constraint';

throws-like { apply-one(-> $a, $b { }) }, X::TypeCheck::Binding::Parameter,
    'a pointy block with too many parameters still fails the constraint';
throws-like { apply-one(-> { }) }, X::TypeCheck::Binding::Parameter,
    'an empty pointy block still fails a one-parameter constraint';

sub apply-int(&g:(Int)) { g(3) }
my $typed-seen = 0;
apply-int(-> Int $n { $typed-seen = $n });
is $typed-seen, 3, 'typed pointy parameters participate in signature matching';
