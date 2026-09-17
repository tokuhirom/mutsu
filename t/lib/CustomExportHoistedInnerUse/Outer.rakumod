# Mirrors the JSON::Fast / TailNamedCall shape from #8564: a bare-file module
# whose only top-level statement is a non-unit `module` block that `use`s
# another module with a custom `sub EXPORT`, and a `sub` hoisted to the head
# of that SAME block (mutsu hoists every `SubDecl` so forward references
# resolve, running `RegisterDecl` before the block's own in-position `use`
# executes) calls the imported symbol. The env-only alias a custom `sub
# EXPORT` installs has no registry entry to fall back on, so it must already
# be live by the time the hoisted sub registers, not only after the
# in-position `use` re-runs EXPORT.
module Outer {
    use InnerCustomExport;
    sub caller-fn() is export {
        inner-thing();
    }
}
