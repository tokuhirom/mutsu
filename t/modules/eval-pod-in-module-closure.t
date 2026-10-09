use lib 't/lib';
use Test;
use EvalPodInClosure;

# Reduced from Pod::TreeWalker: an EVAL'd string is its own compilation unit
# with its own `$=pod`. Inside a closure of a module method, mutsu read the
# calling module's (empty) document instead, so `$=pod[0]` was Any.

plan 6;

my $o = EvalPodInClosure.new;
is $o.direct('a').contents[0].contents[0], 'a', 'EVAL in a module method sees its own $=pod';
my @m = $o.mapped(<x y>);
is @m.elems, 2, 'EVAL in a map closure of a module method yields blocks';
is @m[1].contents[0].contents[0], 'y', 'each block holds its own text';
my @p = $o.via-private(<p q>);
isa-ok @p[0], Pod::Block::Named, 'private method called from a closure';
is @p[1].contents[0].contents[0], 'q', 'private method result content';
is $=pod.elems, 0, 'the mainline $=pod is untouched';
