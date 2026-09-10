use Test;

# The bundled OpenSSL battery: `use OpenSSL` must resolve from the shipped
# `modules/` tree with NO `-I` (zero-config), and the genuine community binding
# must run on mutsu far enough to build a TLS client context. This exercises the
# whole NativeCall stack the binding needs — `is native(&ssl-lib)` (library name
# from a code object), the `is repr('CStruct')` opaque handles (SSL_METHOD /
# SSL_CTX / SSL returned and passed as pointers), and the qualified-name native
# dispatch. No network is used; a real `https://` request is a separate manual
# smoke test (needs connectivity).

plan 3;

# Zero-config load from the bundled modules/ tree (no -I).
my $loaded = try { EVAL 'use OpenSSL; 1' };
ok $loaded, 'use OpenSSL loads from the bundled modules/ tree with no -I';

# Building a client context drives SSL_library_init / TLS_client_method /
# SSL_CTX_new / SSL_new through NativeCall. Skip gracefully if the host has no
# system libssl (the crypto rides the OS; a bare CI image might lack it).
my $ctx;
my $err;
{
    use OpenSSL;
    $ctx = try { OpenSSL.new(:client) };
    $err = $!;
}

if !$ctx.defined && $err && ~$err ~~ /'Cannot locate native library'/ {
    skip 'system libssl not installed', 2;
} else {
    ok $ctx.defined, 'OpenSSL.new(:client) builds a TLS client context';
    is $ctx.^name, 'OpenSSL', 'the context is an OpenSSL instance';
}
