# Grammar parse preserves user high-water marks

Failed grammar parses no longer write a dynamic variable named `$*HIGHWATER`
while computing their diagnostics. A grammar's own `ws` method or token wrapper
now controls that variable, including on a failed parse.
