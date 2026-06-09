use Test;

plan 8;

# Assigning to an immutable literal must throw X::Assignment::RO
# with a rakudo-matching "Cannot modify an immutable <Type> (<repr>)" message.
throws-like '120 = 3', X::Assignment::RO,
    message => /'Cannot modify an immutable Int (120)'/;
throws-like '1.0 = 3', X::Assignment::RO,
    message => /'Cannot modify an immutable Rat (1)'/;
throws-like '1e0 = 3', X::Assignment::RO,
    message => /'Cannot modify an immutable Num (1)'/;
throws-like '"a" = 3', X::Assignment::RO,
    message => /'Cannot modify an immutable Str (a)'/;

# Assigning to the return value of a non-rw sub is also read-only.
throws-like 'sub f() { 42 }; f() = 3', X::Assignment::RO;

# Negative cases: real assignments must still work.
lives-ok { my $x = 1; $x = 2 }, 'scalar assignment is fine';
lives-ok { my @a = 1, 2; @a[0] = 9 }, 'array element assignment is fine';
lives-ok { my %h; %h<k> = 5 }, 'hash element assignment is fine';
