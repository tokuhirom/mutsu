# RakuAST::StatementList.new accepts positional statements

`RakuAST::StatementList.new(stmt, ...)` now takes its statements positionally, the form Rakudo's
own `.raku` output prints, so a pasted `Q|...|.AST` dump can be rebuilt and `EVAL`ed. Each argument
must be a RakuAST node; `.add-statement` keeps working on a populated list. Closes #10760.
