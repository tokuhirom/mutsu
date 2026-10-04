# Split the runtime module declarations

The runtime root now keeps its module declarations in smaller files grouped by
area. New runtime modules can be registered alongside related modules without
editing the same long declaration block in `src/runtime/mod.rs`. Module paths
and visibility remain unchanged.
