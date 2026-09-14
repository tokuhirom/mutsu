# `elsif` accepts a condition without intervening whitespace

The parser now accepts `elsif($condition)` as a conditional branch, matching
Rakudo. Red 0.2.5 uses this spelling in
`MetamodelX::Red::Relationship`, so the module no longer stops at that branch
when loading. This closes one parser gap found while working through
[#7988](https://github.com/tokuhirom/mutsu/issues/7988).
