# A Hash is an element of the Array built from it

`%h ∈ @p` after `my @p = %h, %h` answered False once a `Slangify` slang was
imported: `values_identical` compared two Hashes by node address only, while
`===` and `.WHICH` use the never-reused `which_id`. A Hash now also counts as
identical to another that reports the same `.WHICH` id (#17325).
