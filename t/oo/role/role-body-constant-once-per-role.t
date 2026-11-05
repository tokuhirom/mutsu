use Test;

plan 5;

# A role body's `my constant` is initialised once, not once per composing class
# (#16255): every class composing the role, and every method of it, reads the
# one binding. A `my` variable of the body is still per composition.
my @inits;
role Local {
    my constant TOKEN = do { @inits.push('init'); Mu.new };
    my @classes = $?CLASS.^name;
    method token { TOKEN.WHICH }
    method classes { @classes }
}
class V does Local {}
class U does Local {}
class W does Local {}

is V.new.token, U.new.token, 'two classes composing a role share one constant';
is U.new.token, W.new.token, 'and so does a third';
is V.new.token, V.new.token, 'repeated reads see the same object';
is @inits.elems, 1, 'the initializer ran once';
is-deeply (V.new.classes, U.new.classes), (['V'], ['U']),
    'a `my @x` of the body is still computed per composing class';
