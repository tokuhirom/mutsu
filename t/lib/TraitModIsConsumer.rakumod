use TraitModIsProvider;

# `is Singleton` here is the named trait argument of the provider's
# trait_mod:<is>, in a module that imported it itself (the main program
# imports nothing).
class TraitModIsConsumer is Singleton {
    method hi { "hi from the singleton" }
}
