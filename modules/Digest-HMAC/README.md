Raku-Digest-HMAC
==============

Fork from [@retupmoca's original](https://github.com/retupmoca/P6-Digest-HMAC) fixing some installation errors.

## Example Usage ##

    use Digest::HMAC;
    use Digest;

    my Buf $hmac = hmac($key, $data, &md5);
    my Str $hmac = hmac-hex($key, $data, &md5);

## Functions ##

 -  `sub hmac-hex($key, $data, Callable &hash, $blocksize = 64 --> Str)`

    Returns the hex stringified output of hmac.

 -  `sub hmac($key, $data, Callable &hash, $blocksize = 64 --> Buf)`

    Computes the HMAC of the passed information.

    `$key` and `$data` can either be Str or Blob objects; if they are Str they
    will be encoded as ascii.

    `&hash` needs to be a hash function that takes and returns a Blob or Buf. If
    it operates on or returns a Str, it will not work. (The md5, sha1, sha256 functions
    from Digest work well, as in the example above)

    `$blocksize` is the block size of the hash function. 64 is the default, and
    is correct for at least md5, sha1, sha256.

## See also

Since version 0.28, [`Digest`](https://raku.land/zef:grondilu/Digest) also
 implements the HMAC algorithm, along with many others. You can use that one
  for a more complete range of digest methods.
