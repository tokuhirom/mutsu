use Test;

# #18778: `.Capture` of a type object, as rakudo's `Mu.Capture` (which reads
# the public attributes) and the classes' own `:D` `Capture`s answer it.

plan 14;

is-deeply Mu.Capture, \(), 'Mu';
is-deeply Any.Capture, \(), 'Any';
is-deeply Cool.Capture, \(), 'Cool';
is-deeply Nil.Capture, \(), 'Nil inherits Mu.Capture';
is-deeply Exception.Capture, \(), 'a core class without attributes';

throws-like { Rat.Capture }, Exception,
    message => "Cannot look up attributes in a Rat type object. Did you forget a '.new'?",
    'a core class with attributes';
throws-like { List.Capture }, X::Parameter::InvalidConcreteness,
    message => /"must be an object instance of type 'List'"/, 'a :D Capture refuses the type object';

class P { has $!x = 1 }
is-deeply P.Capture, \(), 'a user class with only private attributes';
class Q { has @.a }
throws-like { Q.Capture }, Exception,
    message => "Cannot look up attributes in a Q type object. Did you forget a '.new'?",
    'a user class with a public attribute';
class Q2 is Q { }
throws-like { Q2.Capture }, Exception,
    message => "Cannot look up attributes in a Q2 type object. Did you forget a '.new'?",
    '...inherited';
class S is Rat { }
throws-like { S.Capture }, Exception, message => /'a S type object'/, 'a subclass of a core class';
class T is List { }
throws-like { T.Capture }, X::Parameter::InvalidConcreteness,
    message => /"type 'List'," \s+ "not a type object of type 'T'"/, '...whose Capture is :D';
role R { }
is-deeply R.Capture, \(), 'a role without attributes';
is-deeply (class { has $.x = 1 }).new.Capture, \(:x(1)), 'an instance is unchanged';
