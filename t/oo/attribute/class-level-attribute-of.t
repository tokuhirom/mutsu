use Test;

plan 3;

# BDD::Behave::Playwright declares its shared failure list as `my @.list of Failure`.
class TypedClassAttribute {
    my @.items of Int;
}

is TypedClassAttribute.items.WHAT.gist, '(Array[Int])',
    'a class-level array attribute keeps its postfix element type';
is TypedClassAttribute.items.elems, 0,
    'a typed class-level array attribute starts empty';
dies-ok { TypedClassAttribute.items.push('not an Int') },
    'a class-level array attribute enforces its postfix element type';
