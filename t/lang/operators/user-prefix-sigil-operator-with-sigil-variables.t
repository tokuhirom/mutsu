use Test;

# Found via Chemistry::Stoichiometry, which imports Math::Matrix's
# `prefix:<%>` / `prefix:<@>`. With such an operator in scope the parser probes
# whether a `%`/`@` begins an ordinary sigil variable; that probe used to
# re-enter the scope table while it was borrowed and panicked
# ("RefCell already borrowed").

plan 7;

sub prefix:<%>(Str $s) { "p:$s" }
sub prefix:<@>(Str $s) { "a:$s" }

my %h = a => 1;
my @a = 1, 2;

is %h<a>, 1, '%h<a> is still a hash variable';
is %h{'a'}, 1, '%h{...} subscripts';
is (%h).raku, '{:a(1)}', 'parenthesised hash variable';
is @a[0], 1, '@a[0] is still an array variable';
my %x = %h;
is %x.elems, 1, 'hash variable on the right of an assignment';
is @a.map({ %(a => $_) }).raku, '({:a(1)}, {:a(2)}).Seq', '%(...) contextualizer in a block';
is [%h, %x].elems, 2, 'hash variables in an array composer';
