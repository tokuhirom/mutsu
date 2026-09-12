use v6;
use Test;

# A role method's parameter type may name a sibling type by the name it is
# written with in the source, which is RELATIVE to the enclosing package:
# `unit class SA` registers a nested `class Column::List` as
# `SA::Column::List`, but every signature in the file spells it
# `Column::List`.
#
# mutsu validates a role method's parameter types when the role is composed,
# and qualified the name with each enclosing package only for an UNQUALIFIED
# constraint, with the undecorated base name never substituted. So a
# relatively-qualified name — with or without a coercion/definedness
# decoration — was rejected as
# "Invalid typename 'Column::List(Any)' in parameter declaration."
# `SQL::Abstract`'s `role Distinction` is exactly this shape; measured by
# https://github.com/tokuhirom/mutsu/issues/7993.

plan 4;

class SA {
    class Column::List {
        has $.elems;
        method COERCE($x) { self.new(:elems($x)) }
    }

    role Distinction {
        method plain(Column::List $c) { 'plain:' ~ $c.elems }
        method coerced(Column::List(Any) $c) { 'coerced:' ~ $c.elems }
        method definite(Column::List:D $c) { 'definite:' ~ $c.elems }
    }

    class Distinction::Columns does Distinction { }
}

my $obj = SA::Distinction::Columns.new;

is $obj.plain(SA::Column::List.new(:elems('a'))), 'plain:a',
    'a relatively-qualified sibling type resolves in a role method signature';
is $obj.coerced('b'), 'coerced:b',
    'the same name decorated as a coercion type resolves too';
is $obj.definite(SA::Column::List.new(:elems('c'))), 'definite:c',
    'the same name with a :D smiley resolves too';

throws-like { EVAL 'role RNope { method f(No::Such::Type $x) { } }; class CNope does RNope { }' },
    X::Parameter::InvalidType,
    'a genuinely undeclared qualified name is still rejected';
