use Test;

# `Metamodel::MethodContainer.declares_method` is a local declaration probe,
# rather than an MRO lookup.  Red uses it while composing model roles to tell
# whether a model supplied its own BUILD or TWEAK method.

class DeclaresMethodBase {
    method own { }
    submethod boot { }
    method !hidden { }
    has $.accessor;
}

class DeclaresMethodChild is DeclaresMethodBase {
    method child { }
}

role DeclaresMethodRole {
    method role-method { }
}

class DeclaresMethodComposed does DeclaresMethodRole { }

grammar DeclaresMethodGrammar {
    token TOP { 'x' }
}

is DeclaresMethodBase.^declares_method('own'), 1,
    'a directly declared method is reported';
is DeclaresMethodBase.^declares_method('boot'), 1,
    'a directly declared submethod is reported';
is DeclaresMethodBase.^declares_method('accessor'), 1,
    'a public attribute accessor is reported';
is DeclaresMethodBase.^declares_method('hidden'), 0,
    'a private method is not reported';
is DeclaresMethodBase.^declares_method('missing'), 0,
    'an absent method is not reported';

is DeclaresMethodChild.^declares_method('child'), 1,
    'a child method is reported on its declaring type';
is DeclaresMethodChild.^declares_method('own'), 0,
    'an inherited method is not a local declaration';
is DeclaresMethodChild.^declares_method('accessor'), 0,
    'an inherited accessor is not a local declaration';

is DeclaresMethodComposed.^declares_method('role-method'), 1,
    'a method composed from a role is reported on the composing class';
is DeclaresMethodGrammar.^declares_method('TOP'), 1,
    'a grammar token is reported';
is DeclaresMethodGrammar.^declares_method('parse'), 0,
    'an inherited grammar method is not a local declaration';

is DeclaresMethodBase.HOW.declares_method(DeclaresMethodBase, 'own'), 1,
    'the HOW call form has the same result as the caret form';

done-testing;
