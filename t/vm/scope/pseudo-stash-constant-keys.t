use Test;

# A sigil-less key of a frame-walking pseudo-stash names a term, so it finds a
# sigil-less `constant` (stored under its term key) as well as a lexical type
# (#17846).

plan 6;

constant foo = 3;

sub dynamic-foo { DYNAMIC::<foo> }
is dynamic-foo(), 3, 'DYNAMIC::<foo> finds a caller\'s constant';

sub caller-bar { CALLER::<bar> }
sub declares-bar { constant bar = 5; caller-bar() }
throws-like { declares-bar() }, X::Caller::NotDynamic, symbol => 'bar',
    'CALLER::<bar> of a constant is not dynamic, named as spelled';

sub caller-type { CALLER::<C> }
sub declares-type { my class C { }; caller-type() }
throws-like { declares-type() }, X::Caller::NotDynamic, symbol => 'C',
    'CALLER::<C> of a lexical class is not dynamic, named as spelled';

sub holds-type { my class C { }; DYNAMIC::<C> }
is holds-type().^name, 'C', 'DYNAMIC::<C> still finds a lexical class';

sub caller-missing { CALLER::<no-such-thing> }
is-deeply caller-missing(), Nil, 'a key no caller declares is Nil';

my $*dyn = 4;
sub caller-dyn { CALLER::<$*dyn> }
is caller-dyn(), 4, 'a dynamic variable is still read';
