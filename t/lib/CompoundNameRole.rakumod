unit module CompoundNameRole;

# TAP.rakumod's exact shape: a `role` establishes the package, a `my role` with
# a compound name installs its last component into that package, and a class in
# the same file composes it through the fully qualified name.
role Entry { }

my role Entry::Handler {
    method handle-entry() { 'handled' }
}

my class State does CompoundNameRole::Entry::Handler { }

sub make-state() is export { State.new }
