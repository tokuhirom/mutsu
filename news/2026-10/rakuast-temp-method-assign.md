`temp` method and attribute lvalues now round-trip through RakuAST's ordinary
prefix and assignment nodes. Attribute spelling is retained separately from
method calls, and compound assignments preserve their operator while using the
existing save/restore implementation. The new structural and evaluation tests
cover public and private attributes, `self` calls, plain assignment, and
compound assignment.
