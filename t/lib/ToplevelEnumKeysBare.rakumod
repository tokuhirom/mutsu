# A package-less module file: its top-level enum is a GLOBAL symbol, so its
# keys are visible to the importer. Used by
# t/modules/module-toplevel-enum-keys-off-frame-env.t.
enum TEKSettings (:TEK-A(1) :TEK-B(2));

sub tek-a() is export { TEK-A }

class TEKHolder {
    my enum TEKState <TEK-Waiting TEK-Done>;
    method done { TEK-Done }
    method closure { -> { TEK-Waiting } }
    method later { supply { emit TEK-Done } }
}
