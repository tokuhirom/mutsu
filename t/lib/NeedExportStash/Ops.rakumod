unit module NeedExportStash::Ops;
multi sub infix:<↱>(Mu $l, Mu $r) is equiv(&infix:<x>) is export { "p($l,$r)" }
sub plain-export($x) is export { "plain($x)" }
