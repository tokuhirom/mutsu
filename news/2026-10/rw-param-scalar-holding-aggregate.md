# `is rw` parameters alias a `$` variable that holds a Hash or Array

A `$` variable is a Scalar container whatever it holds, but mutsu only aliased
it for an `is rw` parameter when it held a plain value. Holding a Hash, an
Array, a List or an object, the variable was handed over bare:

```raku
sub w($p is rw) { $p = 5 }
my $h = {}; w($h); say $h;        # was {}, now 5

sub r($p is rw) { return-rw $p }
my $r = {}; r($r) = 1; say $r;    # was "expects a writable container", now 1
```

The positional light call path now boxes such a variable into its shared cell
(`capture_rw_arg_cell`), `return-rw` of a parameter whose slot already holds
the caller's cell hands that cell out instead of the aggregate inside it, and
the argument list the parser relays to an lvalue call (`f($h) = 1`, `++f($h)`)
binds each `$` variable's container through the new `CaptureRwArgCell` opcode.
Together with the rebind fix of #10361 this makes `TOML::Thumb`'s `walk-key`
(rebind an `is rw` pointer down a table path, then assign through
`return-rw`) work (#11077).
