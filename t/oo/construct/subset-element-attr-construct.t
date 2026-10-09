use Test;

plan 6;

# A lowercase user `subset` as the element type of an `@`/`%` attribute is
# checked at construction, like an uppercase class element type.

subset ne of Str where *.chars > 0;
class SEA-A { has ne @.r }
class SEA-H { has ne %.r }

dies-ok { SEA-A.new(r => ("a", "")) }, 'array attr rejects a failing element';
lives-ok { SEA-A.new(r => ("a", "b")) }, 'array attr accepts passing elements';
dies-ok { SEA-H.new(r => {a => ""}) }, 'hash attr rejects a failing value';
lives-ok { SEA-H.new(r => {a => "x"}) }, 'hash attr accepts passing values';
is SEA-A.new(r => ("a", "b")).r.elems, 2, 'accepted elements are kept';
is SEA-A.new.r.elems, 0, 'unset attribute stays empty';

done-testing;
