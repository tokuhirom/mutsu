# A `sub EXPORT` hook module whose operators come from its EXPORT::MANDATORY
# stash, bound in a loop at run time (the annotations distribution's shape).
module WordsHook { use WordsHook::Core; }
my package EXPORT {
    package MANDATORY {
        OUR.WHO.BIND-KEY: .key, .value for WordsHook::Core::;
    }
}
sub EXPORT(*@args) { Map.new }
