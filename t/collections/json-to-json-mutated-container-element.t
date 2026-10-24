use v6;
use Test;

# A hash element mutated in place (`%h<k>.push(..)`) is boxed in a container
# cell; `to-json` serialized the cell as a quoted string ("1 2") instead of the
# array it holds (mutsu#14870, Pakku::Recman::Ecosystem's derived index).
plan 5;

my %a; %a<k>.push(1); %a<k>.push(2);
is Rakudo::Internals::JSON.to-json(%a), '{"k":[1,2]}', 'an autovivified array pushed twice';

my %b = k => [1, 2]; %b<k>.push(3);
is Rakudo::Internals::JSON.to-json(%b), '{"k":[1,2,3]}', 'a literal array pushed to';

my %c; %c<h><k>.push({ a => 1 }); %c<h><k>.push({ a => 2 });
is Rakudo::Internals::JSON.to-json(%c), '{"h":{"k":[{"a":1},{"a":2}]}}', 'nested, with hashes';

my @d = [1], [2]; @d[0].push(9);
is Rakudo::Internals::JSON.to-json(@d), '[[1,9],[2]]', 'an array element pushed to';

for %c.kv -> $k, %v {
  is Rakudo::Internals::JSON.to-json(%v), '{"k":[{"a":1},{"a":2}]}', 'through a for-loop hash parameter';
}
