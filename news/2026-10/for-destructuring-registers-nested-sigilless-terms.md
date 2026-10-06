Sigilless parameters inside destructured `for` signatures are now registered as
terms throughout the loop body, so a trailing name like `q` cannot be parsed as
a quote language and the following declarations remain visible.
