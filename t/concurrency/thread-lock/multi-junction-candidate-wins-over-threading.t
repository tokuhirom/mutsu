use Test;

plan 4;

role Ex { }
class I does Ex { }

# A candidate that takes the Junction whole beats autothreading, even when
# another candidate declares `Any:D` for the same position.
multi g(Ex:D $l, Junction $j) { "junction" }
multi g(Ex:D $l, Any:D $e) { "any" }
is g(I.new, 1|2), 'junction', 'the Junction candidate receives the Junction';
is g(I.new, 5), 'any', 'a plain value still takes the Any:D candidate';

multi k(Ex:D $l, Any:D $e) { "any" }
is k(I.new, 1|2).gist, 'any(any, any)', 'with no Junction candidate it threads';

multi m(Int $a, Mu $b) { "mu" }
multi m(Int $a, Any:D $b) { "any" }
is m(1, 1|2), 'mu', 'a Mu candidate also takes the Junction whole';
