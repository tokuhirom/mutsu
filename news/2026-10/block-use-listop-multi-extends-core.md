# A block-scoped `use` of a core listop multi keeps the core forms

`{ use M; splice(@a, 1, 2) }` died with "Cannot resolve caller splice(Array...)" when
`M` exports a protoless `multi splice` for another signature: the block's lexical
dispatcher held only the module's candidates and hid CORE's array forms. A protoless
family named after one of the seven core array listops now keeps the registry path,
which extends CORE's dispatch set (ADR-0044 D1). Part of #14140.
