# RakuAST preserves superseded class and role HOWs

When a unit uses `EXPORTHOW::SUPERSEDE`, RakuAST round trips now retain the
custom HOW for its built-in `class` and `role` declarations. The source marker
stays out of the rendered RakuAST tree, and unrelated units keep their default
HOW (#14576).
