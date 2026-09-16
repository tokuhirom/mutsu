use v6;
use Test;

# From Lumberjack::Dispatcher::Syslog: independently declared class enum
# members must not poison one another, and each method keeps its own member.
class First {
    enum FirstLevel <Error>;
    method level() { Error }
}

class Second {
    enum SecondLevel <Error>;
    method level() { Error }
}

plan 2;
is First.new.level, First::FirstLevel::Error,
    'a class method resolves its own enum member after another class declares the same name';
is Second.new.level, Second::SecondLevel::Error,
    'a later class enum member is not a poisoned alias';
