# Anonymous enum inside a role body

An anonymous `enum <A B>` in a role body no longer raises `X::Declaration::OurScopeInRole`: it installs no package symbol, and rakudo accepts it. Named and explicit `our` enums are still rejected. This unblocks loading `LibXML::Raw::DOM::Node`, `::Document` and `::Element`.
