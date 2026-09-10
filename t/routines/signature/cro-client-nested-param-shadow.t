use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

# Regression test for a cross-thread shared-variable clobber: a method's own
# TYPED parameter (e.g. Cro::HTTP::Client's private
# `!get-pipeline(Cro::Uri $url, ...)`) shares a bare name with an unrelated
# lexical live in an awaiting `start { ... }` block (`my $url` in the test
# script). Once the cross-thread shared-variable store is active, a nested
# spawn happening *inside* the shadowing call (e.g. a `Promise.in(...)`
# started deep in Cro's connection-establishment code) force-published the
# callee's shadowed `$url` value into the store under the same bare name,
# and the caller's next `await` pulled the wrong value back over its own
# `$url` -- corrupting it permanently after the first HTTP request.
#
# Root cause and fix: src/runtime/runtime_shared_vars.rs
# (`mask_thread_redeclared_params` / `ThreadParamMask` /
# `unmask_thread_redeclared_params`), consulted from
# src/runtime/runtime_thread.rs (`clone_for_thread_excluding`) and applied at
# every method/sub call boundary in src/vm/vm_method_dispatch.rs
# (`call_compiled_method`) and src/vm/vm_call_named_inner.rs
# (`call_compiled_function_named_inner`).
#
# A from-scratch pure-Raku minimal repro was not found after extensive
# investigation (the trigger needs the exact combination of: a nested spawn
# happening while a same-named parameter is live, a shared-store dirty-mark
# on the *caller's* lexical, and an env-based -- not local-slot-based --
# read of it back in the caller; see the investigation notes referenced from
# the PR this test shipped with). Pinning against the real vendored
# Cro::HTTP::Client instead, per CLAUDE.md's testing conventions, is the
# fallback when a minimal repro proves elusive.
#
# Requires the vendored Cro::HTTP dist tree under tmp/cro-work/ (built by a
# local scratch script; not part of the committed tree). Automatically
# skipped when absent -- including in CI, where this scratch directory is
# never populated.

my $inc-file = 'tmp/cro-work/inc-paths.txt'.IO;

unless $inc-file.e {
    plan :skip-all<Cro::HTTP vendored dist not present under tmp/cro-work/ (local-only regression test; see tmp/cro-suite-run.sh)>;
}

plan 1;

my @compiler-args = $inc-file.slurp.words;

my $code = q:to/RAKU/;
use Cro::HTTP::Router;
use Cro::HTTP::Server;
use Cro::HTTP::Client;

constant TEST_PORT = 31427;
my $url = "http://localhost:{TEST_PORT}";

my $app = route {
    get -> 'route' {
        content 'text/plain', 'GET response';
    }
}

my $service = Cro::HTTP::Server.new(:host('localhost'), :port(TEST_PORT), application => $app);
$service.start;

my $p = start {
    my $r1 = (await Cro::HTTP::Client.get("$url/route")).status;
    my $before2 = $url;
    my ($r2, $err);
    try {
        $r2 = (await Cro::HTTP::Client.get("$url/route")).status;
        CATCH { default { $err = .message; } }
    }
    say "RESULT r1=$r1 before2=$before2 r2={$r2 // 'undef'} err={$err // ''}";
    CATCH { default { say "RESULT caught: {.message}"; } }
}
# Await the block itself instead of a fixed `sleep` -- this must pass under
# both the fast release binary (`make roast`) and the much slower debug
# binary (`make test`, ADR-0014) without a fragile hardcoded timing budget.
await $p;
$service.stop;
RAKU

is_run $code,
    { out => /'RESULT r1=200 before2=http://localhost:31427 r2=200'/, status => 0 },
    :@compiler-args,
    'a second Cro::HTTP::Client request inside the same start{} block does not corrupt an outer $url lexical';
