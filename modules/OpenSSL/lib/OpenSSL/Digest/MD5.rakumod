use OpenSSL::NativeLib;
use NativeCall;

constant MD5-CTX-SIZE      = 92;
constant MD5_DIGEST_LENGTH = 16;

class OpenSSL::Digest::MD5
{
    has $!context;

    sub MD5_Init(Blob)                 returns int32 is native(&gen-lib) { * }
    sub MD5_Update(Blob, Blob, size_t) returns int32 is native(&gen-lib) { * }
    sub MD5_Final(Blob, Blob)          returns int32 is native(&gen-lib) { * }

    submethod BUILD() {
        $!context = buf8.allocate(MD5-CTX-SIZE);
        self.init;
    }

    method init() {
        MD5_Init($!context) or die "Bad Call to MD5_Init";
        return self;
    }

    multi method add(Str $msg) {
        self.add($msg.encode('ascii'));
    }

    multi method add(Blob $msg) {
        MD5_Update($!context, $msg, $msg.bytes) or die "Bad Call to MD5_Update";
        return self;
    }

    method addfile(Str $filename, Int $bufsiz = 64 * 1024) {
        my $fh = open($filename, :bin) or die "open $filename";
        LEAVE { .close with $fh }
        self.add($fh.read($bufsiz)) while not $fh.eof;
        return self;
    }

    method hash() {
        my $digest = buf8.allocate(MD5_DIGEST_LENGTH);
        MD5_Final($digest, $!context) or die "Bad Call to MD5_Final";
        self.init;
        return $digest;
    }

    method hex() {
        self.hash.list».fmt("%02x").join;
    }
}

# vim: expandtab shiftwidth=4
