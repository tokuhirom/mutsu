unit module AnonDoesRole;

# An exported role, referenced by its short alias from the importing scope.
role Searchable is export {
    method search(*@) { ... }
}

# An exported base class used as the `but role` invocant.
class Repo is export {
    method plugins { [] }
}
