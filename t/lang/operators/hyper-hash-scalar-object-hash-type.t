use Test;

plan 8;

# A symbolic hyper op (`>>op>>`/`<<op<<`) between an object hash and a
# scalar must preserve the hash's object-hash identity (key_type /
# original_keys) -- the key set does not change, only the values, so the
# result should still report `Hash[Any,Any]`, not degrade to a plain Hash.
# Regression: `hyper_op_pair`'s hash-scalar branches built a fresh plain
# HashData, dropping `key_type`/`value_type`/`declared_type`/`original_keys`.

my %a{Any} = a => 1, b => 2, c => 3;

# The scalar operand must be on a dwim-adjacent side to broadcast, so only
# these arrow combinations are valid hash-vs-scalar hyper ops (verified
# against `raku`; the others die with X::HyperOp::NonDWIM).
is (%a >>~>> 3).WHAT.raku, 'Hash[Any,Any]', '%h{Any} >>op>> scalar keeps object-hash type';
is (%a <<~>> 3).WHAT.raku, 'Hash[Any,Any]', '%h{Any} <<op>> scalar keeps object-hash type';
is (3 <<~<< %a).WHAT.raku, 'Hash[Any,Any]', 'scalar <<op<< %h{Any} keeps object-hash type';
is (3 <<~>> %a).WHAT.raku, 'Hash[Any,Any]', 'scalar <<op>> %h{Any} keeps object-hash type';

is-deeply (%a >>~>> 3).raku, '(my Any %{Any} = :a("13"), :b("23"), :c("33"))',
    '%h{Any} >>op>> scalar renders as a typed object hash';

# A key-type-mismatched key object (non-Str) is preserved through the op too.
my %o{Any} = 1 => "x", 2 => "y";
is-deeply %o.keys.map(*.^name).sort.list, ("Int", "Int"), 'sanity: %o has Int keys';
is (%o >>~>> "!").WHAT.raku, 'Hash[Any,Any]', '%h{Any} with non-Str keys keeps object-hash type through hyper op';
is-deeply (%o >>~>> "!").keys.map(*.^name).sort.list, ("Int", "Int"),
    'non-Str original keys survive the hyper op';
