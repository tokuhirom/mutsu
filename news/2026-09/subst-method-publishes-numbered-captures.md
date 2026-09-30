# `.subst` publishes numbered captures to `$0`..

After `Str.subst(/(b)/, ...)`, `$/` was set but the `$0`, `$1`, ... entries
were left stale (or Nil), including after a closure replacement, which restores
them around its call. The method form now publishes them through the same
`publish_subst_capture_env` that `s///` uses, on the regex, native-fast-path and
closure-replacement routes.
