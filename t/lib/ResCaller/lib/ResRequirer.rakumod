unit class ResRequirer;

# Loads ResDist lazily from inside a routine, the way HTTP::UserAgent loads
# IO::Socket::SSL when a request turns out to be https.
method load-greeting() {
    try require ::("ResDist");
    ::('ResDist').greeting;
}
