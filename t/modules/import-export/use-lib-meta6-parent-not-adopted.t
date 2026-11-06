use Test;
use lib 't/fixtures/meta6-parent-not-adopted/earlier';
# `dist/t` is not a distribution root: the META6.json of its parent `dist/` is not consulted for it.
use lib 't/fixtures/meta6-parent-not-adopted/dist/t';
use Meta6Parent::M;

plan 2;

is who(), 'earlier', 'the earlier use lib entry provides the module, not the parent dist via its META6.json';

# A directory that holds the META6.json itself still resolves `provides`.
my $out = run($*EXECUTABLE, '-I', 't/fixtures/meta6-parent-not-adopted/dist', '-e', 'use Meta6Parent::M; print who()', :out).out.slurp;
is $out, 'dist-lib-Real', 'a repository whose prefix holds the META6.json resolves provides';
