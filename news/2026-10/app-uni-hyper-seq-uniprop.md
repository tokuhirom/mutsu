# App::Uni: hyper.map result Seqs, uniprop Age/East_Asian_Width, fudge counts

Taking App::Uni from red to green exposed four general gaps:

- A `.hyper.map` callback returning a Seq a sibling item still reads
  (`$s .= grep(...)` under a lock) was reified on the worker thread while the
  sibling consumed it, failing with "iterator of this Seq is already in use".
  Result Seqs are now reified after the join.
- `uniprop(..., 'Age')` always answered `1.1`; it now reads a table generated
  from rakudo.
- `East_Asian_Width` answers the short alias (`W`, `Na`) as rakudo does.
- `uniprop` property names match without regard to case or underscores
  (`white_space`).
- The roast fudge preprocessor ignored the count in `#?rakudo.moar N todo`.
