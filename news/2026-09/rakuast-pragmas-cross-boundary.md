`use strict`, `use fatal`, and the other argument-less core pragmas now cross
mutsu's RakuAST boundary as direct `RakuAST::Pragma` nodes. Their constructors,
accessors, and EVAL lowering are supported while ordinary module imports remain
separate work.
