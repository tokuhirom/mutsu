# A local `&name` binding shadows an imported code variable

`my &unpack = ...` or `state &unpack = ...` in a routine did not shadow an
`our &unpack is export` the file imported: the call read the block's import
set first and ran the import (PDF's `XRef.decode-index`, #14561). A `&name`
binding visible at the call site now takes precedence over the import sets.
