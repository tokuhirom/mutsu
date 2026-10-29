unit module EhCycOps;
use EhCycMid;
multi sub infix:<↱>(Mu $l, Mu $r) is equiv(&infix:<x>) is export { "parse($l,$r)" }
