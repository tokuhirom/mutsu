# `RakuAST::Blockoid.new` builds an empty Blockoid

`RakuAST::Blockoid.new` with no arguments answers a Blockoid over an empty `StatementList`, as
rakudo does (#17147); it used to die asking for a single StatementList.
