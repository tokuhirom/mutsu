unit module LightClosureOpMod;

class Money is export {
    has $.n;
    method amount { $!n }
}

multi sub infix:<+>(Money $a, Money $b) is export {
    Money.new(n => $a.n + $b.n)
}
