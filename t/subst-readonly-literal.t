use Test;

plan 5;

# A destructive s/// that matches a string literal has no writable container,
# so it must throw X::Assignment::RO (Raku behavior).
dies-ok { 'abc' ~~ s/b/g/ }, 's/// on a matching string literal dies';
throws-like { 'abc' ~~ s/b/g/ }, X::Assignment::RO,
    's/// on a string literal throws X::Assignment::RO';

# A non-matching s/// on a literal is a no-op and must NOT throw.
lives-ok { 'xyz' ~~ s/b/g/ }, 'non-matching s/// on a literal does not throw';

# Substitution on a real variable still works and writes back.
{
    my $x = 'abc';
    $x ~~ s/b/X/;
    is $x, 'aXc', 's/// writes back to a variable';
}

# tr/// on a matching string literal also dies.
dies-ok { 'abc' ~~ tr/b/X/ }, 'tr/// on a matching string literal dies';
