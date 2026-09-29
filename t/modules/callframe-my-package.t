use Test;

# From the Sub::Name distribution: callframe(N).my<::?PACKAGE> names the
# package the frame's code was running in.
plan 5;

sub inner { callframe(2).my<::?PACKAGE>.^name }
sub outer { inner() }

is outer(), 'GLOBAL', 'top-level caller is GLOBAL';
package Foo { is outer(), 'Foo', 'caller inside a package'; }
package Foo { package Bar { is outer(), 'Foo::Bar', 'caller inside nested package'; } }

sub direct { callframe(1).my<::?PACKAGE>.^name }
is direct(), 'GLOBAL', 'callframe(1) at top level';
package Foo { is direct(), 'Foo', 'callframe(1) inside a package'; }
