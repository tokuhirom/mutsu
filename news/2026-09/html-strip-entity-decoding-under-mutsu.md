# HTML::Strip entity decoding now matches Rakudo

`HTML::Strip` now passes all 27 of its baseline assertions under mutsu. The
interpreter preserves NQP's native null when a missing `nqp::atkey` result is
rebound, and implements the `nqp::eqatic` and `nqp::radix` primitives used by
`HTML::Entity::Fast` for unknown and numeric HTML entities.
