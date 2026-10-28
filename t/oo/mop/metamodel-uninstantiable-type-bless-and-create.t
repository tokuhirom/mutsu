use Test;

# An `is repr<Uninstantiable>` type object has no instances: not through
# `.new`, `.bless` nor `.CREATE`.
plan 3;

class K is repr<Uninstantiable> { }

dies-ok { K.new }, '.new dies';
dies-ok { K.bless }, '.bless dies';
dies-ok { K.CREATE }, '.CREATE dies';
