unit class Leak::Sub::Walker is rw;
method walk(*%s) { for <type>.map({ 1 }) { }; True }
