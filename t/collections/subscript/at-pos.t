use Test;

plan 7;

class PositionalThing {
    method AT-POS($i) { "at($i)" }
    method EXISTS-POS($i) { $i == 7 || $i == 9 }
}

my $obj = PositionalThing.new;

is $obj[3], "at(3)", '[] dispatches to AT-POS';
ok $obj[7]:exists, ':exists dispatches to EXISTS-POS for existing index';
nok $obj[3]:exists, ':exists dispatches to EXISTS-POS for missing index';
is-deeply $obj[7, 3, 9]:exists, (True, False, True), 'slice :exists dispatches to EXISTS-POS per index';
is-deeply $obj[7, 3, 9]:exists:kv, (7, True, 9, True), ':exists:kv filters by actual existence';
is-deeply $obj[7, 3, 9]:exists:p, (7 => True, 9 => True), ':exists:p returns pairs for existing indices';
is-deeply $obj[7, 3, 9]:!exists, (False, True, False), ':!exists negates EXISTS-POS results';
