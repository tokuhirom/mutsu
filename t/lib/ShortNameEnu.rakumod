use ShortNameHdr;

class ShortNameEnu {
    my enum Expecting <RequestLine Header Body>;

    method check() {
        my $e = Header;
        given $e {
            when Header { "enum-ok" }
            default { "enum-fail: " ~ $e.raku }
        }
    }
}
