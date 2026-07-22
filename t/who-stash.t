use v6;
use Test;

plan 12;

# .WHO on builtin values (not just type objects) answers with the type's
# Stash, same as Rakudo (42.WHO.^name is Stash, not Hash).

is 42.WHO.^name, 'Stash', '.WHO on an Int value is a Stash';
is "x".WHO.^name, 'Stash', '.WHO on a Str value is a Stash';
my @a = 1, 2;
is @a.WHO.^name, 'Stash', '.WHO on an Array instance is a Stash';
is @a.WHO.gist, 'Array', '.WHO gist of an Array instance is the package name';
my %h = a => 1;
is %h.WHO.^name, 'Stash', '.WHO on a Hash instance is a Stash';
is (@a>>.WHO).WHAT.gist, '(Stash)', '>>.WHO applies to the target and is a Stash';

# Stash.raku renders the symbols hash literal (Stash is a Hash subclass in
# Rakudo), not the generic `Stash.new`.

is 42.WHO.raku, '{}', '.raku of an empty Stash is {}';
class Foo { class Bar {} }
is Foo.WHO.raku, '{:Bar(Foo::Bar)}', 'Stash.raku lists the nested package symbols';
is Foo.WHO.gist, 'Foo', 'Stash.gist is the package name';
is Foo.new.WHO.^name, 'Stash', '.WHO on a user-class instance is a Stash';
is Foo.new.WHO.raku, '{:Bar(Foo::Bar)}', 'instance .WHO.raku matches the type stash';

# The pseudo-package colon form still aliases .WHO.
ok Foo:: === Foo.WHO, 'Foo:: is the same stash as Foo.WHO';

done-testing;
