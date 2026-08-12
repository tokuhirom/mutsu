# Also has no `unit module`, so loading it records ITS distribution under the
# same generic "GLOBAL" package key that ResBareA's load used — the collision
# t/nested-closure-resources-file-attribution.t exercises.
sub bare-b-noop() is export { }
