# Used by t/modules/eval-use-module-lexical-class.t.
my class Hid { method hi { "hid" } }
my role Mark { }
sub make-hid() is export { Hid.new.hi }
sub install(Mu:U $c) is export {
    $c.^add_method("viaadd", method { Hid.new.hi });
    $c.^add_method("marked", method ($x) { $x ~~ Mark });
}
