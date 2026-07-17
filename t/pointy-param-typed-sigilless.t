use v6;
use Test;

plan 8;

# A pointy-block parameter can be sigilless (\name) AND carry a type
# constraint: `-> Mu \type { ... }`. The parser used to accept the two only
# separately (`-> \t` / `-> Mu $t`), failing on the combination — which is how
# JSON::Unmarshal writes every `where` lambda (`subset ClassLike of Mu
# where -> Mu \type { ... }`), blocking `use JSON::Unmarshal` entirely.

# expression position (lambda assigned to a variable)
my &f = -> Int \t { t + 1 };
is f(41), 42, 'expression-position pointy with typed sigilless param';

# for-loop position
my @got;
for 1, 2 -> Mu \t { @got.push(t) }
is-deeply @got, [1, 2], 'for-loop pointy with typed sigilless param';

# subset where clause taking such a lambda (the JSON::Unmarshal shape)
subset PosInt of Mu where -> Mu \type { type ~~ Int && type > 0 };
ok 42 ~~ PosInt, 'subset where pointy lambda accepts';
nok -1 ~~ PosInt, 'subset where pointy lambda rejects by predicate';
nok "x" ~~ PosInt, 'subset where pointy lambda rejects by type expr';

# multi-line layout, as written in the wild
subset Defined of Mu
    where -> Mu \type { type.defined };
ok 42 ~~ Defined, 'multi-line subset where pointy lambda (defined)';
nok Mu ~~ Defined, 'multi-line subset where pointy lambda (type object)';

# the type constraint is enforced, not just parsed
my &g = -> Int \t { t };
dies-ok { g("x") }, 'typed sigilless pointy param still type-checks';
