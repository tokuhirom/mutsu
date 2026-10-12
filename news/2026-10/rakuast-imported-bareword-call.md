Imported functions used as bare terms now retain their resolved call shape in
RakuAST. Conversion emits `Call::Name::WithoutParentheses`, and lowering
reuses the existing bare-call path. The new structural test checks the node and
EVAL result; the imported-call subscript regression test passes in RakuAST
mode.
