unit class Leak2::Sub::Walker is rw;
method walk(*%s) { for <type>.map({ 1 }) { }; True }
