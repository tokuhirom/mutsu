# RakuAST: `use Foo:from<NQP>`

The `:from` adverb of a module name now renders as rakudo does: a
`ColonPair::Value` (key `from`, a word `QuotedString`) in the colonpairs of the
`Statement::Use` module `Name`, instead of being refused as "an argument and
import tags at once". Lowering splits it back into the parser's argument and
`from` tag, so the NQP no-op `use` keeps working under `MUTSU_RAKUAST=1`.
One more `t/` file passes (ADR-10723 S10). Other module adverbs (`:ver`,
`:auth`) still render as identifier text and are unchanged.
