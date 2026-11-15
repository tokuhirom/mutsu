# EVAL in a method declares lexical classes in the method's package

A plain EVAL now executes in the same declaring package used to compile its
snippet. A `my class X` EVALed from a method is therefore named `Owner::X`,
including through an inherited method. An explicit EVAL context and REPL
package still take precedence (#17609).
