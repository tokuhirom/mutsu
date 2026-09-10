use v6;
use lib 't/lib';
use Test;
use EvalLoadingHelper;

# #7836: a routine records its declaring file as it is created, and a module's
# own top-level mainline runs via `run_block` and pushes NO routine frame. So
# the frame walk that stamps the file reached straight past the mainline to
# whatever routine was still on the stack underneath -- the CALLER's frame, in a
# different compunit entirely when the load came from a string EVAL inside
# another module's routine. `Test`'s `use-ok` is exactly that shape
# (`EVAL "use $module"`).
#
# Every `sub` the loaded module declared was then stamped with the invoking
# compunit's file, and since `enter_compilation_unit` anchors `current_unit`
# from that stamp on every call, the module's own `sub EXPORT` could not resolve
# its own class. That is what broke `Terminal::ANSI::OO`, and with it
# `Log::Async/08-use.rakutest` in the bundled-library gate.

plan 3;

# The load itself must succeed: `sub EXPORT` runs during it and names the
# module's own class qualified.
is EvalLoadingHelper::load-via-eval('EvalLoadedExporter'), 'ok',
        "a module's EXPORT resolves its own class when EVAL-loaded from another module";

# Loading it a second time re-runs the remembered EXPORT (its map may depend on
# the `use` arguments), down a different code path -- which must be anchored the
# same way.
is EvalLoadingHelper::load-via-eval('EvalLoadedExporter'), 'ok',
        'and again on the re-`use` path, which re-runs the remembered EXPORT';

# Not just EXPORT: an ordinary `our sub` of that module carries the same stamp,
# so calling one after such a load must reach the module's own class too.
use EvalLoadedExporter;
is plain-helper(), 'exporter',
        "an ordinary sub of that module still reaches its own compunit's class";
