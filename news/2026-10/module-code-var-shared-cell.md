# A module's `my &var` written by an exported sub reaches mainline closures

A module like this registered a filter closure at load time over a
module-level `my &backend`, and let the importer set the backend later through
an exported sub:

```raku
my &backend;
our sub register-backend(:&handler!) is export { &backend = &handler }
register-filter :name<md>, :handler(-> $body { &backend.defined ?? backend($body) !! 'MISSING' });
```

The exported sub saw its own write, but the filter closure kept reading the
undefined `&backend` and answered `MISSING`. Template::HAML's markdown filter
tests died this way. A module-level `$` lexical that an `our sub` reads or
writes is boxed into a shared cell at its declaration, so the sub and every
closure over it alias one container. A `&` code variable was excluded from that
boxing, and so was a variable whose initial value is already a `Sub`. Both now
take the cell (#11051).
