RakuAST tree evaluation now reads the tree's lexicals through `UNIT::` even
after an import changes the active environment. A routine in the tree sees
later writes to those lexicals.
