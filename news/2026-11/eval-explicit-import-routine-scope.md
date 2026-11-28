An explicit `import` inside an AST EVAL unit now lets the unit call routines that the import binds in its lexical block. The undeclared-routine check still judges calls outside that block.
