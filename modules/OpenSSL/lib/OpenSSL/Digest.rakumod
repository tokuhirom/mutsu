use OpenSSL::NativeLib;
use NativeCall;

my str @hex = BEGIN (^256)>>.fmt("%02x");
my sub hexify(Blob:D $blob) { $blob.map({ @hex[$_] }).join }

#- constants -------------------------------------------------------------------
our constant MD5_DIGEST_LENGTH    = 16;
our constant SHA1_DIGEST_LENGTH   = 20;
our constant SHA224_DIGEST_LENGTH = 28;
our constant SHA256_DIGEST_LENGTH = 32;
our constant SHA384_DIGEST_LENGTH = 48;
our constant SHA512_DIGEST_LENGTH = 64;

#- links to the native library functions ---------------------------------------
my sub MD5(    Blob, size_t, Blob ) is native(&gen-lib) { ... }  # UNCOVERABLE
my sub SHA1(   Blob, size_t, Blob ) is native(&gen-lib) { ... }  # UNCOVERABLE
my sub SHA224( Blob, size_t, Blob ) is native(&gen-lib) { ... }  # UNCOVERABLE
my sub SHA256( Blob, size_t, Blob ) is native(&gen-lib) { ... }  # UNCOVERABLE
my sub SHA384( Blob, size_t, Blob ) is native(&gen-lib) { ... }  # UNCOVERABLE
my sub SHA512( Blob, size_t, Blob ) is native(&gen-lib) { ... }  # UNCOVERABLE

#- md5 -------------------------------------------------------------------------
my proto sub md5(|) is export {*}
my multi sub md5(Str() $string --> Blob:D) {
    md5 $string.encode
}
my multi sub md5(IO::Path:D $io --> Blob:D) {
    md5 $io.slurp(:bin)
}
my multi sub md5(Blob:D $msg --> Blob:D) {
     my $digest := blob8.allocate(MD5_DIGEST_LENGTH);
     MD5($msg, $msg.bytes, $digest);
     $digest
}
my sub md5-hex(Any:D $source --> Str:D) is export { hexify md5 $source }

#- sha1-------------------------------------------------------------------------
my proto sub sha1(|) is export {*}
my multi sub sha1(Str() $string --> Blob:D) {
    sha1 $string.encode
}
my multi sub sha1(IO::Path:D $io --> Blob:D) {
    sha1 $io.slurp(:bin)
}
my multi sub sha1(Blob:D $msg --> Blob:D) {
     my $digest := blob8.allocate(SHA1_DIGEST_LENGTH);
     SHA1($msg, $msg.bytes, $digest);
     $digest
}
my sub sha1-hex(Any:D $source --> Str:D) is export { hexify sha1 $source }

#- sha224 ----------------------------------------------------------------------
my proto sub sha224(|) is export {*}
my multi sub sha224(Str() $string --> Blob:D) {
    sha224 $string.encode
}
my multi sub sha224(IO::Path:D $io --> Blob:D) {
    sha224 $io.slurp(:bin)
}
my multi sub sha224(Blob:D $msg --> Blob:D) {
     my $digest := blob8.allocate(SHA224_DIGEST_LENGTH);
     SHA224($msg, $msg.bytes, $digest);
     $digest
}
my sub sha224-hex(Any:D $source --> Str:D) is export { hexify sha224 $source }

#- sha256 ----------------------------------------------------------------------
my proto sub sha256(|) is export {*}
my multi sub sha256(Str() $string --> Blob:D) {
    sha256 $string.encode
}
my multi sub sha256(IO::Path:D $io --> Blob:D) {
    sha256 $io.slurp(:bin)
}
my multi sub sha256(Blob:D $msg --> Blob:D) {
     my $digest := blob8.allocate(SHA256_DIGEST_LENGTH);
     SHA256($msg, $msg.bytes, $digest);
     $digest
}
my sub sha256-hex(Any:D $source --> Str:D) is export { hexify sha256 $source }

#- sha384 ----------------------------------------------------------------------
my proto sub sha384(|) is export {*}
my multi sub sha384(Str() $string --> Blob:D) {
    sha384 $string.encode
}
my multi sub sha384(IO::Path:D $io --> Blob:D) {
    sha384 $io.slurp(:bin)
}
my multi sub sha384(Blob:D $msg --> Blob:D) {
     my $digest := blob8.allocate(SHA384_DIGEST_LENGTH);
     SHA384($msg, $msg.bytes, $digest);
     $digest
}
my sub sha384-hex(Any:D $source --> Str:D) is export { hexify sha384 $source }

#- sha512 ----------------------------------------------------------------------
my proto sub sha512(|) is export {*}
my multi sub sha512(Str() $string --> Blob:D) {
    sha512 $string.encode
}
my multi sub sha512(IO::Path:D $io --> Blob:D) {
    sha512 $io.slurp(:bin)
}
my multi sub sha512(Blob:D $msg --> Blob:D) {
     my $digest := blob8.allocate(SHA512_DIGEST_LENGTH);
     SHA512($msg, $msg.bytes, $digest);
     $digest
}
my sub sha512-hex(Any:D $source --> Str:D) is export { hexify sha512 $source }

#- hack ------------------------------------------------------------------------
# To allow version fetching
module OpenSSL::Digest:ver<0.2.7>:auth<raku-community-modules> { }

# vim: expandtab shiftwidth=4
