# `nqp::gethllsym('Raku', 'COMPILER-FRONTEND')` reports `rakuast`

Rakudo's compiler binds the frontend it runs on under that key and distributions branch on it. Red's `t/88-projection-chains` marks its interpolation subtests `todo` when it reads `rakuast`; mutsu returned `Nil`, so the helper picked the `legacy` branch, ran the subtests as hard failures and the file exited non-zero. mutsu's frontend is RakuAST (ADR-10723), so the interpreter now seeds the binding with `rakuast`, as Rakudo's default frontend does.
