use Test;

plan 4;

module Sto {
    my class Client {
        method who { 'private' }
    }

    our sub private-client-name() is export {
        Client.^name
    }
}

is Sto::private-client-name(), 'Sto::Client',
    'the declaring package can still use its lexical class';
dies-ok { Sto::Client.^name },
    'a my class inside a package is not a package-qualified symbol outside it';
ok !Sto::.keys.grep(* eq 'Client'),
    'a my class is absent from the package stash';

class Outer::Thing {
    method who { 'outer' }
}

module Outer {
    my class Thing is Outer::Thing { }

    our sub consumer-state() is export {
        Thing.new.who ~ '/' ~ Thing.^name
    }
}

is Outer::consumer-state(), 'outer/Outer::Thing',
    'a package-qualified parent resolves to the outer class, not the lexical child';
