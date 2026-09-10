use Test;

# Coherence pin for the env<->locals single-store path
# (MUTSU_NO_BLANKET_RECONCILE): the right-hand thunk of a zip short-circuit
# metaop (`Zandthen`/`Zorelse`) that writes a captured-outer lexical
# (`$x = $_`, `$x++`) must reach the caller's local slot, not only the env
# entry that the (removed) blanket reconcile used to pull. Must pass
# identically with and without the blanket reconcile.

plan 5;

{
    my Mu $side-effect is default(Nil) = 0;
    23 Zandthen ($side-effect = $_,);
    is $side-effect, 23, "Zandthen topicalizing thunk write reaches the slot";
}

{
    my Mu $side-effect is default(Nil) = 0;
    1 Zandthen ($side-effect++,);
    is $side-effect, 1, "Zandthen increment thunk reaches the slot";
}

{
    my Mu $side-effect is default(Nil) = 0;
    Nil Zandthen ($side-effect++,);
    is $side-effect, 0, "Zandthen does not run the thunk for an undefined left";
}

{
    my Mu $side-effect is default(Nil) = 0;
    Nil Zorelse ($side-effect = $_,);
    is $side-effect, Nil, "Zorelse topicalizing thunk write reaches the slot";
}

{
    my Mu $side-effect is default(Nil) = 0;
    Nil Zorelse ($side-effect++,);
    is $side-effect, 1, "Zorelse increment thunk reaches the slot";
}
