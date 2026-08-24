# Hyper calls can recursively invoke the current block

Hyper callable-method syntax now resolves `&?BLOCK` to the current block callable instead of stringifying a missing code-variable value into an empty method name. This lets a dynamic hyper call such as `».&?BLOCK` invoke the current block for each element through the existing VM-native callable dispatch.

`for` bodies normally execute as inline compound bytecode, so they previously had no callable object to expose as `&?BLOCK`. The compiler now materializes a closure only for a `for` body that references `&?BLOCK`, stores it in a hidden local, and the VM installs that callable on the block stack while each inline iteration runs. Ordinary loop execution remains inline, preserving loop control, parameter writeback, and phaser handling, while recursive calls execute the equivalent compiled closure.

A focused TAP regression covers named subs, callable variables, and deterministic recursive invocation of an inline `for` block through hyper dispatch.
