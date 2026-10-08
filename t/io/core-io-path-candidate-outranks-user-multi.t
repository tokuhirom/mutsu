use Test;

# From the Data::Importers distribution: a module-local `multi slurp` whose
# first parameter is untyped with a `where` clause must not intercept
# `slurp($io-path)`. The setting's IO::Path-typed candidate is narrower, so it
# wins and the `where` clause never runs (checked against rakudo).
plan 5;

sub is-url(Str $url --> Bool) { $url.starts-with('http') }

multi sub slurp($source where $source.&is-url, :$format = Whatever) { 'url' }
multi sub dir($source where $source.&is-url, :$format = Whatever) { 'url' }

sub read-it(IO::Path $file, :$extra) { slurp($file) }

my $path = $?FILE.IO;
is slurp($path).chars > 10, True, 'slurp(IO::Path) reaches the core candidate';
is read-it($path).chars > 10, True, 'also from a sub with a named parameter';
is slurp('http://example.com'), 'url', 'a URL string still reaches the user candidate';
is dir($path.parent).elems > 0, True, 'dir(IO::Path) reaches the core candidate';

# `lines` has an untyped core signature: the user candidate wins and its
# `where` clause runs (and dies on a non-Str), as in rakudo.
multi sub lines($source where $source.&is-url) { 'url' }
dies-ok { lines($path) }, 'untyped core signature: user where candidate is tried first';
