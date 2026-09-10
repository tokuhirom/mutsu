use Test;

plan 8;

my %h = a => 1, b => 2, c => 3;

is-deeply %h<a b z>:exists, (True, True, False), ':exists works for multi hash keys';
is-deeply %h<a b z>:!exists, (False, False, True), ':!exists works for multi hash keys';

is-deeply %h<a b z>:exists:kv, ('a', True, 'b', True), ':exists:kv filters by existing keys';
is-deeply %h<a b z>:exists:!kv, ('a', True, 'b', True, 'z', False), ':exists:!kv keeps all keys';

is-deeply %h<a b z>:exists:p, (a => True, b => True), ':exists:p returns filtered pairs';
is-deeply %h<a b z>:exists:!p, (a => True, b => True, z => False), ':exists:!p returns all pairs';

is-deeply %h{}:exists, %h{*}:exists, 'empty hash subscript matches {*} for :exists';
is-deeply %h{}:!exists, %h{*}:!exists, 'empty hash subscript matches {*} for :!exists';
