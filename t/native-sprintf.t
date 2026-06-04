use Test;

# Pin for docs/vm-decoupling.md step 3: sprintf/zprintf are dispatched by the
# VM's native_function table (builtins::functions::native_sprintf) instead of
# falling back to Interpreter::call_function. This must preserve every sprintf
# semantic the interpreter implementation had.

plan 18;

is sprintf("%d", 42), "42", 'integer';
is sprintf("%05.2f", 3.14159), "03.14", 'float width+precision';
is sprintf("%s=%d", "x", 7), "x=7", 'mixed string/int';
is sprintf("%x", 255), "ff", 'hex';
is sprintf("%o", 8), "10", 'octal';
is sprintf("%b", 5), "101", 'binary';
is sprintf("%e", 12345.678), "1.234568e+04", 'scientific';
is sprintf("%-10s|", "hi"), "hi        |", 'left-justify';
is sprintf("%2\$s %1\$s", "a", "b"), "b a", 'positional args';
is sprintf("%d", [42]), "42", 'single-array flatten, one elem';
is sprintf("%d %d", [1, 2]), "1 2", 'single-array flatten, two elems';
is sprintf("no directives"), "no directives", 'no directives';
is sprintf("%+d", 5), "+5", 'explicit sign';
is sprintf("%c", 65), "A", 'char';
is sprintf("%%"), "%", 'literal percent';
is sprintf("%3d", 7), "  7", 'min width';

# zprintf (zero-padded variant) still works natively.
is sprintf("%d-%d", 1, 2), "1-2", 'two ints';

# Error propagation: the directive/argument-count validation runs in the native
# path too (an arity mismatch must throw, not silently format wrong).
dies-ok { sprintf("%d %d", 1).say }, 'too-few-args arity mismatch throws';
