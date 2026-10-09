# A rebind of a package-block `my` no longer moves earlier aliases

`my $n := $a; $a := X` inside a routine of `module Foo { my $a ... }` or a class body used to
write X through the container `$n` shared, so `$n` followed the rebind. The by-name store now
replaces the container in the `package_lexicals` store (and a live `@`/`%` entry in `env`) instead
of writing through it, matching Rakudo (#12129).
