use v6;
use Test;

# A paren-less list operator (listop) call must swallow a trailing comma into
# its own argument list, exactly like Rakudo: `f $x,` is `f($x)`, NOT
# `(f($x),)`. When the call returns a container (Array/Hash), the wrong reading
# nests the result on list-assignment (`my @a = f $x,` -> `[[...]]`).
# Regression: Template::Mustache inheritable_partials relied on
# `my @parsed = get-template .<val>, :delims(...), ;` staying flat.

plan 8;

sub arr($k) { return [ { :t($k) }, ]; }

# single-line trailing comma
my @a = arr 'x', ;
is @a.elems, 1, 'trailing comma: elems stays 1 (not nested)';
is @a[0].^name, 'Hash', 'trailing comma: [0] is the Hash, not an Array';

# multiline trailing comma (module form)
my @b = arr
            'y',
            ;
is @b.elems, 1, 'multiline trailing comma: elems stays 1';
is @b[0].^name, 'Hash', 'multiline trailing comma: [0] is the Hash';

# with a named arg then trailing comma
sub arn($k, :$d) { return [ { :t($k), :d($d) }, ]; }
my @c = arn
            'z',
            :d('dd'),
            ;
is @c.elems, 1, 'named arg + trailing comma: elems stays 1';
is @c[0]<t>, 'z', 'named arg + trailing comma: positional bound';
is @c[0]<d>, 'dd', 'named arg + trailing comma: named bound';

# no trailing comma still flat (control)
my @d = arr 'w';
is @d.elems, 1, 'no trailing comma: elems 1 (control)';
