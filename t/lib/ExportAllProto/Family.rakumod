unit module ExportAllProto::Family;
class Driver is export { }
# The proto itself is not exported; only its candidates are.
proto run-with(|c) {
    my $label = c.hash<label> // 'none';
    {
        my $*EAP-LABEL = $label;
        {*}
    }
}
multi run-with(&block, Str :$with = 'default', :$label) is export { "str:$*EAP-LABEL" }
multi run-with(&block, Driver:D :$with, :$label) is export { "driver:$*EAP-LABEL" }
