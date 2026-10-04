use Test;

plan 4;

# `GLOBAL::NAME` reaches a sigil-less constant of the global scope, also
# when the block that declared it has exited (#11518).
{ constant RIS = Int; }
is GLOBAL::RIS.^name, 'Int', 'GLOBAL::NAME reaches a type-valued constant of an exited block';
is RIS.^name, 'Int', 'the bare name reaches it too';

{ constant RIS2 = 42; }
is GLOBAL::RIS2, 42, 'GLOBAL::NAME reaches a value constant of an exited block';

package P { our constant K = 5; }
is P::K, 5, 'Pkg::NAME still reaches a package-scope constant';
