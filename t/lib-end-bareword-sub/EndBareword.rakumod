# The END block names `finish` as a bare term before the sub is declared,
# as Testo's `END { $seen-done or done-testing }` does.
my Bool:D $seen = False;
END { $seen or finish }
sub finish() { note "finish ran"; exit 3 }
sub mark() is export { $seen = True }
