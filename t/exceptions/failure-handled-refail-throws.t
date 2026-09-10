use v6;
use Test;

# Re-failing a HANDLED Failure throws its wrapped exception outright
# (verified against raku); only an unhandled Failure re-arms as a passive
# Failure. DBDish::ErrorHandling's execute error path is the load-bearing
# case:
#
#     with self!handle-errors { ... } else { .fail }
#
# `with` marks the Failure handled (via .defined), so the `.fail` in the
# else branch must surface the exception — DBIish's t/38-pg-errors.rakutest
# "Raise Temporary Exception" hung on a passive Failure instead.

plan 4;

class X::My is Exception { has $.m; method message { $.m } }

class K {
    method !dispatch($ex) { $ex.fail; }
    method !seterr() { self!dispatch(X::My.new(m => "b")); }
    method run() {
        with self!seterr() { "never" } else { .fail }
    }
}

my $k = K.new;

throws-like {
    my $r = $k.run;
    CATCH { default { .rethrow } }
}, X::My, 'handled-Failure .fail throws through a CATCH-rethrow block';

throws-like {
    my $r = $k.run;
}, X::My, 'handled-Failure .fail throws without a CATCH too';

# Direct form: a Failure handled by .defined, then re-failed, throws.
sub s() { X::My.new(m => "direct").fail }
my $f = s();
ok $f.handled === False || $f.defined === False, 'Failure starts unhandled / undefined';
throws-like { $f.fail }, X::My, 're-failing the now-handled Failure throws';
