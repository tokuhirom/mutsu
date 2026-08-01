# The goal of this method is to guarantee the resultant child path is
# inside the invocant. We resolve the path completely, so for that to
# happen, the kid cannot be inside some currently non-existent dirs, so
# this method will fail with X::IO::Resolve in those cases. To find out
# if the kid is in fact a kid, we fully-resolve the kid and the
# invocant. Then, we append a dir separator to invocant's .absolute and
# check if the kid's .absolute starts with that string.

my sub child-secure (IO::Path:D \SELF, \child) is export {
    (my $kid := SELF.child(child).resolve: :completely) ~~ Failure
      ?? $kid
      !! (my $res-self := SELF.resolve: :completely) ~~ Failure
        ?? $res-self  # failed to resolve invocant, return the Failure
        !! ($_ := $res-self.absolute ~ SELF.SPEC.dir-sep) eq
            $kid.absolute.substr(0, .chars)
          ?? $kid     # kid appears to be kid-proper; return it. Otherwise fail
          !! Failure.new:
               X::IO::NotAChild.new:
                 :path($res-self.absolute), :child($kid.absolute)
}

=begin pod

=head1 NAME

IO::Path::ChildSecure - Secure version of IO::Path.child

=head1 SYNOPSIS

=begin code :lang<raku>

use IO::Path::ChildSecure;

# good; you get IO::Path
"foo".IO.&child-secure: 'meow';

# still good if 'foo/meow/foo/bar/../' exists; Failure if it doesn't
"foo".IO.&child-secure: 'meow/foo/bar/../meow';

# bad; path isn't a child; you get Failure
"foo".IO.&child-secure: '../';

=end code

=head1 DESCRIPTION

In the Raku Programming Language,
L<IO::Path.child|https://docs.raku.org/type/IO::Path#method_child> isn't
secure, in a sense that it does no checks for whether the resultant path is
actually a child of the original path.

This module provides a subroutine that can be used as an alternative that
B<will> check whether the resultant path is a child of the original path.

=head1 EXPORTED SUBROUTINES

=head2 child-secure

=begin code :lang<raku>

"foo".IO.&child-secure: 'meow'; # good; you get IO::Path
"foo".IO.&child-secure: 'meow/foo/bar/../meow'; # still good
"foo".IO.&child-secure: '../';  # bad; path isn't a child; you get Failure

child-secure "foo".IO, '../';  # can also use as a proper sub

=end code

Appends the given path chunk to the invocant and ensures the resultant path
is, in fact, a child of the invocant, by accessing the filesystem and
fully-resolving the path. The last chunk of the resultant path does not have
to exist for the resolution to succeed.

Will L<fail|https://docs.raku.org/routine/fail> with C<X::IO::Resolve> if
failed to fully resolve the resultant path or with C<X::IO::NotAChild> if
the resultant path is not a child of the invocant.

=head1 SPECIAL NOTES

If you don't need to ensure secureness, use the much-faster core
  L<C<IO::Path.add> method|https://docs.raku.org/type/IO::Path#method_add>

=head1 AUTHOR

Zoffix Znet

=head1 COPYRIGHT AND LICENSE

Copyright 2017-2018 Zoffix Znet

Copyright 2019-2022 Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod

# vim: expandtab shiftwidth=4
