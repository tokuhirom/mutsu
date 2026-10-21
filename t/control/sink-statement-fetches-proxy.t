use Test;

plan 4;

# A statement whose value is a Proxy is sunk, and sinking a Proxy reads it:
# the FETCH runs even though nothing uses the value.
my @log;
sub mk { Proxy.new(FETCH => method () { @log.push("fetch"); 1 }, STORE => method ($) { }) }

mk();
is @log.elems, 1, "a sub call in statement position FETCHes";

class Holder {
    has $.slot;
    method put { $!slot := mk(); Nil }
    method give { mk() }
}
my $h = Holder.new;
@log = ();
$h.give;
is @log.elems, 1, "a method call in statement position FETCHes";

@log = ();
my $kept = mk();
is @log.elems, 1, "an assignment reads it once";

sub dies-on-fetch { Proxy.new(FETCH => method () { die "fetched" }, STORE => method ($) { }) }
throws-like { dies-on-fetch(); 1 }, X::AdHoc, "a FETCH that dies is raised from a sunk statement";
