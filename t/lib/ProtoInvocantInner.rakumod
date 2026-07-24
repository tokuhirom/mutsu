unit class ProtoInvocantInner;

# A `proto method` in its OWN compilation unit. Reaching it from another module's
# method takes the slow proto-dispatch path (same-file protos compile to bytecode
# and never hit it), which is where the invocant-clobber regression lived.
has @.fields;

proto method field(|) {*}

multi method field(*%fields) {
    for %fields.sort(*.key) -> (:key($k), :value($v)) {
        @.fields.push: ($k => $v);
    }
}

multi method field($f) {
    @.fields.first(*.key eq $f)
}
