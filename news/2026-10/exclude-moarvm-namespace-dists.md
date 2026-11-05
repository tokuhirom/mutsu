# Exclude the MoarVM:: namespace distributions from the ecosystem queue

Following the maintainer decision on #15808, distributions whose name carries `MoarVM::`
(`App::MoarVM::Debug`, `App::MoarVM::HeapAnalyzer::Model`, alongside the already excluded
`MoarVM::Bytecode`, `MoarVM::Profile` and `MoarVM::Remote`) are tied to MoarVM's own internals
and need not load or run on mutsu. They are listed in `ecosystem/exclude.txt`, so the
ordered queue no longer offers them. No deprecated core `from-json` was added.
