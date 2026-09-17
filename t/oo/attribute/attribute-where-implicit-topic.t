use Test;

plan 2;

class TruthyAttribute {
    has $.value where .so;
}

is TruthyAttribute.new(value => 1).value, 1,
    'an attribute where-clause can use the implicit topic';

class AllCoolAttribute {
    has @.args where .all ~~ Cool;
}

is AllCoolAttribute.new(args => ['-e', 'say $*IN.get.uc']).args.elems, 2,
    'an attribute where-clause can call a method on the implicit topic';
