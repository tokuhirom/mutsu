use v6;
use Test;

plan 8;

# PLAN §8.15 leftover: an IO::Spec::* *instance* reprs as the generic
# instance form, not the type object's; and catdir/catfile are slurpy.

my $s = IO::Spec::Unix.new;
is $s.gist, 'IO::Spec::Unix.new', 'IO::Spec::Unix instance gists as .new form';
is $s.raku, 'IO::Spec::Unix.new', 'IO::Spec::Unix instance .raku is the .new form';
is $s.canonpath('a//b/'), 'a/b', 'instance path methods still work';

is $*SPEC.gist.substr(0, 1), '(', '$*SPEC is a type object and gists as (...)';

is $*SPEC.catdir(<a b>), 'a/b', 'catdir flattens a passed list';
is $*SPEC.catdir('a', 'b'), 'a/b', 'catdir with separate args';
is $*SPEC.catfile(<x y>, 'z.txt'), 'x/y/z.txt', 'catfile flattens a passed list';
is $*SPEC.catdir(), '', 'catdir with no args is empty';

done-testing;
