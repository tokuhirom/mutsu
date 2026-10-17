# A private call or `trusts` can name its class through a `constant` alias

A class body that declares `constant CUS = Outer;` can now reach the private
methods of `Outer` with `$o!CUS::meth` and can write `trusts CUS`. The alias is
read off the class body when its methods are validated (the constant has not run
yet at that point) and, for `trusts`, resolved to the real class when the shell
is published. Before, the call died with `Cannot call private method 'x' on
package CUS because it does not trust ...`.

Found with `Intl::CLDR` 0.7.6, whose `Types::*` modules all use this pattern:
the whole distribution now loads under mutsu. Its remaining failure in
`t/01-spot-check.rakutest` is a separate `is rw` forwarding bug (#13719).
