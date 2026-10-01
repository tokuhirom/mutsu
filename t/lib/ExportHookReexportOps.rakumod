unit module ExportHookReexportOps;
multi sub infix:<↱>(Mu $l, Mu $r) is equiv(&infix:<x>) is export { "parse($l,$r)" }
multi sub prefix:<⮳>(Mu $l) is export { "source($l)" }
