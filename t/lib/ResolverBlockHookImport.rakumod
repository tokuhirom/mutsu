# Fixture for t/rakuast/rakuast-resolver-block-imports.t: a `sub EXPORT` hook
# import, the way Red's `use Red` brings in `&__RED_OPERATOR_LOADED__`.
sub EXPORT { Map.new: '&resolver-hook-mark' => sub { True } }
