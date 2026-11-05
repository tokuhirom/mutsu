use Test;
plan 12;

# `X::Method::NotFound`'s "Did you mean" tip offers a user class's own methods
# (its parents', its roles' and its public attribute accessors) beside the
# built-in ones; a private method is not offered. Expected messages are
# rakudo 2026.09's.
sub msg(&c) { try c(); $!.message }

role R { method rolemeth {} }
class P { method parentm {}; method alpha {} }
class C is P does R { has $.attr; has $!priv; method alpha2 {}; method !hidden {} }

is msg({ C.new.alphe }),
    "No such method 'alphe' for invocant of type 'C'. Did you mean any of\nthese: 'alpha', 'alpha2'?",
    'own and inherited methods';
is msg({ C.new.alpa }),
    "No such method 'alpa' for invocant of type 'C'. Did you mean any of\nthese: 'alpha', 'all', 'alpha2'?",
    'user and built-in names ranked together';
is msg({ C.new.parentn }),
    "No such method 'parentn' for invocant of type 'C'. Did you mean\n'parentm'?",
    'a parent class method';
is msg({ C.new.rolemeht }),
    "No such method 'rolemeht' for invocant of type 'C'. Did you mean\n'rolemeth'?",
    'a composed role method';
is msg({ C.new.attrr }),
    "No such method 'attrr' for invocant of type 'C'. Did you mean 'attr'?",
    'a public attribute accessor';
is msg({ C.new.hiddn }),
    "No such method 'hiddn' for invocant of type 'C'",
    'a private method is not offered';
is msg({ C.alpha3 }),
    "No such method 'alpha3' for invocant of type 'C'. Did you mean any of\nthese: 'alpha', 'alpha2'?",
    'on the type object';
is msg({ C.new.zzzzzz }),
    "No such method 'zzzzzz' for invocant of type 'C'",
    'nothing close';
class Solo { method solo-method {} }
is msg({ Solo.new.solo-methd }),
    "No such method 'solo-methd' for invocant of type 'Solo'. Did you mean\n'solo-method'?",
    'a class with one method';
is msg({ Solo.new.say2 }),
    "No such method 'say2' for invocant of type 'Solo'. Did you mean 'say'?",
    'a Mu method of a user class';
is msg({ Any.say2 }),
    "No such method 'say2' for invocant of type 'Any'. Did you mean 'say'?",
    'a Mu method on Any';
is msg({ Mu.say2 }),
    "No such method 'say2' for invocant of type 'Mu'. Did you mean 'say'?",
    'a Mu method on Mu';
