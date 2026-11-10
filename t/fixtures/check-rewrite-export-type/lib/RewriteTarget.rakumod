class Gadget {
    has $.n = 7;
    method CALL-ME() { "gadget {$!n}" }
}
sub EXPORT(|) { %( Gadget => Gadget ) }
