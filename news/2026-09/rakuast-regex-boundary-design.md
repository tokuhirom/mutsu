# RakuAST regex boundary design

Issue [#8033](https://github.com/tokuhirom/mutsu/issues/8033) now has a Proposed
[ADR-0088](../../docs/adr/0088-rakuast-regex-boundary-tree.md). It introduces a
shared source-level regex tree for both RakuAST conversion and execution
lowering, while keeping the current matcher as the execution target during the
migration. This is the design handoff for the follow-up implementation; the
issue remains open until the regex node tree and its lowerer are shipped.
