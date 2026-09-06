unit class AttrDefaultClosureScope;

# The closures below are *lowered* when this class is declared but *run* inside
# whatever frame calls `.new`. They must still resolve this file's lexical subs.
has $.lazy = -> $x { helper($x) };
has %.table =
    direct  => &helper,
    wrapped => (-> $x { helper($x) }),
    nested  => (-> $x { -> $y { helper($y) } });

sub helper($x) { "helper($x)" }

method via-attr($x) { $!lazy($x) }
method via-table($k, $x) { %!table{$k}($x) }
method via-nested($x) { %!table<nested>($x)($x) }
method via-local($x) { my $c = -> $y { helper($y) }; $c($x) }
method direct-call($x) { helper($x) }
