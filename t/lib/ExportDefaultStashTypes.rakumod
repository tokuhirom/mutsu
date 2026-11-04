my package EXPORT::DEFAULT {
    class StashPlain { has $.a = 1; method plain { "plain" } }
    class StashMarked is export { has $.b = 6 }
    role StashRole { method r { "role" } }
    enum StashColor <red green>;
    subset StashPos of Int where * > 0;
    our sub stash-helper { 42 }
}
