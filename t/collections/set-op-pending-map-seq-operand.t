use Test;

# From JSON::Mask: `%h.keys (-) @n.map(*.key)` -- an unread `.map` / `.grep`
# Seq as a set-operator operand must be reified, not read as empty.

plan 7;

is (<a b> (-) <a>.map({ $_ })).keys.sort.join(','), 'b', '(-) with a map Seq on the right';
is (<a b> (|) <c>.map({ $_ })).keys.sort.join(','), 'a,b,c', '(|) with a map Seq on the right';
is (<a b> (&) <a>.map({ $_ })).keys.sort.join(','), 'a', '(&) with a map Seq on the right';
is (<a>.map({ $_ }) (-) <a b>).keys.sort.join(','), '', '(-) with a map Seq on the left';
is (<a b> (^) <a>.map({ $_ })).keys.sort.join(','), 'b', '(^) with a map Seq';
ok <a>.map({ $_ }) (<=) <a b>, '(<=) with a map Seq';
ok 'a' (elem) <a b>.map({ $_ }), '(elem) with a map Seq container';
