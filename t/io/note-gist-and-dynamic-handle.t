use Test;

plan 4;

class FakeIO {
    has $.Str is rw = "";

    method print($arg) {
        $!Str ~= $arg;
    }
}

class InterestingGist {
    method gist() { "[ok]" }
}

sub cap(&code) {
    my $*ERR = FakeIO.new;
    code();
    $*ERR.Str;
}

is cap({ note InterestingGist.new }), "[ok]\n", "note uses .gist for objects";

my int $seen;
is cap({ note "" but role { method gist() { $seen = 1; "<x>" } } }), "<x>\n", "note calls .gist for Str values too";
is $seen, 1, "custom .gist on Str was invoked";

is cap({ "flurb".note }), "flurb\n", ".note writes to rebound \$*ERR";
