# Used by t/modules/add-method-module-lexical-type.t (from hide-methods).
my class Hidden { method hi { "hi" } }
my role Marker { }

sub install-on(Mu:U $class) is export {
    $class.^add_method("make-hidden", method { Hidden.new.hi });
    $class.^add_method("is-marked", method ($x) { $x ~~ Marker });
}
