unit role RuntimeUseLazyStubRole;

# A private stub method nobody implements or calls -- Rakudo does not treat
# this as a composition requirement (see role-private-stub-not-required.t).
# Math::Matrix::Util (the real ecosystem dist behind #8806) has this exact
# shape.
method !unimplemented-private-stub { ... }
