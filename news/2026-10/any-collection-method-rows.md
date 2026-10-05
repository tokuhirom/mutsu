Any's scalar collection methods now use built-in method-table rows. `elems`, `end`, `keys`,
`values`, `kv`, `pairs`, `antipairs` and `reverse` share their handlers with the native
cascade, while more-specific List/Map rows and collection-specific fallback behavior remain
unchanged.
