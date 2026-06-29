use Test;

plan 3;

# Calling an instance-only Supply method on the Supply TYPE OBJECT is an
# X::Parameter::InvalidConcreteness (the invocant is constrained to Supply:D).
throws-like q[Supply.skip], X::Parameter::InvalidConcreteness,
    'Supply.skip on the type object dies with InvalidConcreteness';

# Supply CLASS methods on the type object keep working.
lives-ok { Supply.interval(1) }, 'Supply.interval still works on the type object';
lives-ok { Supply.from-list(1, 2, 3) }, 'Supply.from-list still works';

done-testing;
