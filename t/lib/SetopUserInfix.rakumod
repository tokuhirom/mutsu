# Set operators a distribution (Red) overloads for its own type and re-exports
# through an `EXPORT` hook.
use SetopUserInfix::Ops;

sub exports(@experimental --> Map()) {
    SetopUserInfix::Ops::EXPORT::ALL::,
    '&rs' => sub (Str $name) { SetopUserInfix::Ops::Rs2.new(:$name) },
}

multi EXPORT(+@experimental) { exports(@experimental) }
