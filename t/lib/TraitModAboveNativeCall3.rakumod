use TraitModAboveNativeCall2;

# The outermost layer declares `multi trait_mod:<is>` candidates of its own
# (the shape of Cro::HTTP::Router), three module levels above the one that
# loads NativeCall and exports its `&trait_mod:<is>` dispatcher.
role TraitModAboveNativeCall3::Marked { method marked-by() { 'marked' } }

module TraitModAboveNativeCall3 {
    multi trait_mod:<is>(Parameter:D $param, :$marked! --> Nil) is export {
        $param does TraitModAboveNativeCall3::Marked;
    }
    multi trait_mod:<is>(Parameter:D $param, :$tagged! --> Nil) is export {
        $param does TraitModAboveNativeCall3::Marked;
    }

    our sub tmanc3-alive() is export { tmanc2-alive() }
}
