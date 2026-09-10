use v6;
use Test;

plan 15;

# Backtrace::Frame has no custom `.gist` in Rakudo, so both `.gist` and
# `.raku` fall back to the default instance rendering:
#   Backtrace::Frame.new(file => "...", line => N, code => ..., subname => "...")
# `.Str` is a different, deliberately concise rendering
# ("  in block/sub ... at ... line N") and must stay untouched by this.
# Frame *count* and *ordering* legitimately differ between mutsu and rakudo
# (mutsu has no Raku-written CORE setting, so it has no setting frames --
# see todo/tickets/backtrace-has-fewer-frames-than-rakudo.md), so this test
# only asserts on the *shape* of a frame's rendering, never on which frame
# ends up at a given index or on the `code =>` object's memory address.

sub zipi { { { die "Something bad happened" }() }() };
try {
    zipi;
}

my $bt = $!.backtrace;
ok $bt.defined, 'a caught exception carries a Backtrace';
ok $bt.elems > 0, 'the Backtrace has at least one frame';

my $f = $bt[0];
isa-ok $f, Backtrace::Frame, '$bt[0] is a Backtrace::Frame';

# .Str stays the concise "  in ... at ... line N" text, unaffected by the
# .gist/.raku fix. (The exact leading word -- "block"/"sub"/"method" -- is
# gap-2 frame-model territory, so this only checks the surrounding shape.)
ok $f.Str.starts-with('  in ') && $f.Str.contains(' at ') && $f.Str.contains(' line '),
        '.Str keeps its concise "in ... at ... line N" shape';

my $frame_shape = rx/
    ^ 'Backtrace::Frame.new(file => "' <-[ " ]>* '", '
    'line => ' \d+ ', '
    'code => ' .+ ', '
    'subname => "' <-[ " ]>* '")' $
/;

like $f.raku, $frame_shape, '.raku renders the Backtrace::Frame.new(...) attribute shape';
like $f.gist, $frame_shape, '.gist renders the same attribute shape as .raku';
is $f.gist, $f.raku, '.gist and .raku produce the same string';

# .Str must not have regressed into the same shape as .gist/.raku.
isnt $f.Str, $f.gist, '.Str is still the concise text, not the .new(...) form';

# The individual accessors that feed the rendering above must work too.
isa-ok $f.file, Str, '.file returns a Str';
ok $f.file.chars > 0, '.file is non-empty';
isa-ok $f.line, Int, '.line returns an Int';
ok $f.line > 0, '.line is positive';
isa-ok $f.subname, Str, '.subname returns a Str';

my $bool_predicates = so $f.is-hidden ~~ Bool
        and $f.is-routine ~~ Bool
        and $f.is-setting ~~ Bool;
ok $bool_predicates, '.is-hidden / .is-routine / .is-setting all answer Bool';

# .code is documented as "the code object .file/.line point into" -- it
# should at least be defined and stringify without dying.
ok $f.code.defined, '.code is defined';
