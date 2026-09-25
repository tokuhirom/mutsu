use v6;
use Test;

plan 13;

# A parenthesised declarator list is a signature, so each element may carry a
# parameter trait (`my ($a is rw, $b) := ...`). It used to be a parse error
# ("Confused. expected statement"). Issue #9320.

{
    my $x = 1; my $y = 2;
    my ($a is rw, $b is rw) := ($x, $y);
    $a = 5;
    is $x, 5, '`is rw` element aliases the bound container';
    $b = 7;
    is $y, 7, 'the second `is rw` element aliases its own container';
    $x = 9;
    is $a, 9, 'the alias sees later writes to the source';
}

{
    my $x = 1;
    my ($a is raw, $b) := ($x, 2);
    $a = 3;
    is $x, 3, '`is raw` element aliases the bound container';
    dies-ok { $b = 4 }, 'an element without a trait stays read-only';
}

{
    my $x = 1;
    my ($a is readonly) := ($x,);
    dies-ok { $a = 3 }, '`is readonly` element is read-only';
    is $x, 1, 'the source is untouched';
}

{
    # rakudo keeps an `is copy` element of a `my (...)` bind read-only too.
    my $x = 1;
    my ($a is copy) := ($x,);
    is $a, 1, '`is copy` element reads the bound value';
    dies-ok { $a = 11 }, '`is copy` element of a declarator bind is read-only';
}

# rakudo already refuses the bind (X::Parameter::RW); mutsu refuses the write.
dies-ok { my ($a is rw) := (5,); $a = 3 },
    '`is rw` bound to a value without a container cannot be assigned';

{
    my ($a is rw, $b) = 3, 4;
    is "$a $b", '3 4', 'traits are accepted on a list assignment';
}

throws-like 'my (@a is rw) := ([1],)', Exception,
    message => /"don't need 'is rw' to be writable"/,
    '`is rw` on an @ element is refused';
throws-like 'my ($a is foo) := (1,)', Exception,
    message => /"unknown trait 'is' -> 'foo'"/,
    'an unknown trait is refused';
