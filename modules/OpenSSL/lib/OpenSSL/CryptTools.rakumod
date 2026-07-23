use OpenSSL::EVP;
use NativeCall;

unit module OpenSSL::CryptTools;

our proto sub encrypt(|) is export {*}

multi sub encrypt(:$aes256! where .so, |c) is export {
    my $cipher = OpenSSL::EVP::EVP_aes_256_cbc();
    encrypt(:$cipher, |c)
}
multi sub encrypt(:$aes192! where .so, |c) is export {
    my $cipher = OpenSSL::EVP::EVP_aes_192_cbc();
    encrypt(:$cipher, |c)
}
multi sub encrypt(:$aes128! where .so, |c) is export {
    my $cipher = OpenSSL::EVP::EVP_aes_128_cbc();
    encrypt(:$cipher, |c)
}
multi sub encrypt(:$aes128ctr! where .so, |c) is export {
    my $cipher = OpenSSL::EVP::EVP_aes_128_ctr();
    encrypt(:$cipher, |c);
}

multi sub encrypt(Blob $plaintext, :$key, :$iv, :$cipher! where .so) is export {
    my $ctx = OpenSSL::EVP::EVP_CIPHER_CTX_new();
    my $evp = nativecast(OpenSSL::EVP::evp_cipher_st, $cipher);

    if $key.bytes != $evp.key_len {
        die "Key is not {$evp.key_len * 8} bits";
    }
    if $iv.bytes != $evp.iv_len {
        die "IV is not {$evp.iv_len * 8} bits";
    }

    # way bigger than needed
    my $bufsize = $plaintext.bytes * 2;
    $bufsize = 64 if $bufsize < 64;

    OpenSSL::EVP::EVP_EncryptInit($ctx, $cipher, $key, $iv);

    my $part = buf8.new;
    $part[$bufsize] = 0;
    my $partsize = CArray[int32].new;
    $partsize[0] = $bufsize;
    OpenSSL::EVP::EVP_EncryptUpdate($ctx, $part, $partsize, $plaintext, $plaintext.bytes);

    my $out = $part.subbuf(0, $partsize[0]);
    $partsize[0] = $bufsize;

    OpenSSL::EVP::EVP_EncryptFinal($ctx, $part, $partsize);
    $out ~= $part.subbuf(0, $partsize[0]);

    OpenSSL::EVP::EVP_CIPHER_CTX_free($ctx);

    $out
}

our proto sub decrypt(|) is export {*}

multi sub decrypt(:$aes256! where .so, |c) is export {
    my $cipher = OpenSSL::EVP::EVP_aes_256_cbc();
    decrypt(:$cipher, |c)
}
multi sub decrypt(:$aes192! where .so, |c) is export {
    my $cipher = OpenSSL::EVP::EVP_aes_192_cbc();
    decrypt(:$cipher, |c)
}
multi sub decrypt(:$aes128! where .so, |c) is export {
    my $cipher = OpenSSL::EVP::EVP_aes_128_cbc();
    decrypt(:$cipher, |c)
}
multi sub decrypt(:$aes128ctr! where .so, |c) is export {
    my $cipher = OpenSSL::EVP::EVP_aes_128_ctr();
    decrypt(:$cipher, |c);
}

multi sub decrypt(Blob $ciphertext, :$key, :$iv, :$cipher! where .so) is export {
    my $ctx = OpenSSL::EVP::EVP_CIPHER_CTX_new();
    my $evp = nativecast(OpenSSL::EVP::evp_cipher_st, $cipher);

    if $key.bytes != $evp.key_len {
        die "Key is not {$evp.key_len * 8} bits";
    }
    if $iv.bytes != $evp.iv_len {
        die "IV is not {$evp.iv_len * 8} bits";
    }

    # way bigger than needed
    my $bufsize = $ciphertext.bytes * 2;
    $bufsize = 64 if $bufsize < 64;

    OpenSSL::EVP::EVP_DecryptInit($ctx, $cipher, $key, $iv);

    my $part = buf8.new;
    $part[$bufsize] = 0;
    my $partsize = CArray[int32].new;
    $partsize[0] = $bufsize;
    OpenSSL::EVP::EVP_DecryptUpdate($ctx, $part, $partsize, $ciphertext, $ciphertext.bytes);

    my $out = $part.subbuf(0, $partsize[0]);
    $partsize[0] = $bufsize;

    OpenSSL::EVP::EVP_DecryptFinal($ctx, $part, $partsize);
    $out ~= $part.subbuf(0, $partsize[0]);

    OpenSSL::EVP::EVP_CIPHER_CTX_free($ctx);

    $out
}

# vim: expandtab shiftwidth=4
