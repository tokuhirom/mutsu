use Test;

plan 6;

# A store reads the constraint of the variable its own code declares, never a
# caller's same-named one (ADR-13757 section 2.2, #14226).
my @b; sub f() { @b[0] = 'x'; 1 }; sub g() { my Int @b; f() };
is g(), 1, 'element store to an untyped outer @b ignores a caller typed @b';
my %h; sub hf() { %h<k> = 'v'; %h.elems }; sub hg() { my Int %h; hf() };
is hg(), 1, 'element store to an untyped outer %h ignores a caller typed %h';
my $n; sub nf() { $n++; $n }; sub ng() { my Int $n; nf() };
is ng(), 1, 'increment of an untyped outer $n ignores a caller typed $n';

sub t() { my Int $x; $x = 'a'; CATCH { default { return 'typecheck' } } }
is t(), 'typecheck', 'a typed scalar still checks its own store';
sub k() { my %o{Int}; %o{1} = 'a'; %o.keys.head.^name }
is k(), 'Int', 'an object hash keeps its key type';
sub ia() { my Int @a = 1, 2; @a[0]++; @a[0] }
is ia(), 2, 'inc/dec on a typed array element';
