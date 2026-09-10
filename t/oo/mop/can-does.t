use Test;

plan 15;

my @can = 42.can("Str");
ok @can, '.can returns a non-empty list for an existing builtin method';
is @can[0](42), "42", '.can returns callable method objects';
ok !42.can("NoSuchMethod"), '.can returns an empty list for a missing method';

my @meta-can = 42.^can("Str");
ok @meta-can, '.^can returns a non-empty list for an existing builtin method';
is @meta-can[0](42), "42", '.^can returns callable method objects';

role Greeter {
    method greet { "hello" }
}

class Person does Greeter {
}

my $person = Person.new;
ok $person.does(Greeter), '.does returns True for a composed role';
ok $person.does(Person), '.does returns True for the concrete class';
ok $person.does(Any), '.does returns True for parent classes';
ok !$person.does(Str), '.does returns False for unrelated types';
ok $person.can("greet"), '.can sees methods composed from roles';
ok $person.HOW.does($person, Greeter), '.HOW.does sees composed roles on instances';
ok $person.^does(Greeter), '.^does sees composed roles on instances';
ok Person.^does(Greeter), '.^does sees composed roles on type objects';

class BaseWithSubmethod {
    submethod hidden { 42 }
}

class DerivedWithSubmethod is BaseWithSubmethod {
}

ok BaseWithSubmethod.can("hidden"), '.can sees submethods on the declaring class';
ok !DerivedWithSubmethod.can("hidden"), '.can does not inherit submethods into subclasses';
