# Custom export hooks can read tagged stash pairs

Package stashes now support the `:p` adverb used by custom `EXPORT` hooks to
select tagged symbols. This lets modules such as `List::Util` export routines
through `EXPORT::TAG::{...}:p` without needing a native replacement.
