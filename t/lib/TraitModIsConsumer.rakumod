use TraitModIsProvider;

# `is Singleton` here is the named trait argument of the provider's
# trait_mod:<is>, in a module that imported it itself (the main program
# imports nothing). The `use NativeCall` inside a method (as in Pop::Graphics)
# makes NativeCall's own trait_mod:<is> candidates visible, none of which
# matches `:Singleton`: the provider's must still be found.
class TraitModIsConsumer is Singleton {
    method hi { "hi from the singleton" }
    method native-scope {
        use NativeCall;
        1
    }
}
