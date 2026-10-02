# Routine.add_dispatchee on a proto sub

`&proto.add_dispatchee(&other)` now registers every candidate of a named routine as an extra
multi candidate of the proto, so `JSON::Fast::Hyper`-style `BEGIN &to-json-hyper.add_dispatchee(&to-json)`
loads. Closure-carrying anonymous dispatchees (`CLI::Version`, `shorten-sub-commands`) are tracked separately.
