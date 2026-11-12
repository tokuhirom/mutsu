use v6;
use Test;

# `G.new` -- and the start-rule invocant every `G.parse` builds with it --
# constructs a grammar instance like any class instance: attribute defaults,
# named arguments, BUILD and TWEAK, a user `new`. A grammar's built-in
# ancestors (Grammar, Match, Capture) add nothing to build. Repeated, so a
# construction shortcut that skipped a step would show up. Verified against
# Rakudo.

plan 12;

my @log;
grammar Plain { token TOP { <[a..z]>+ } }
grammar WithState {
    has $.limit = 3;
    has @.seen;
    submethod TWEAK { @log.push: "tweak:{$!limit}" }
    token TOP { <word> }
    token word { <[a..z]>+ }
}
grammar OwnNew {
    has $.mode;
    method new(*%args) { self.bless(:mode('custom'), |%args) }
    token TOP { \d+ }
}

ok Plain.new ~~ Plain, 'a plain grammar instance';
ok Plain.new ~~ Match, 'is a Match';
ok (^20).map({ Plain.parse('abc') }).all, 'repeated parses match';

is WithState.new.limit, 3, 'an attribute default applies';
is WithState.new(:limit(5)).limit, 5, 'a named argument reaches its attribute';
is WithState.new.seen.elems, 0, 'an array attribute starts empty';
@log = ();
WithState.new for ^3;
is @log.join(','), 'tweak:3,tweak:3,tweak:3', 'TWEAK runs once per construction';

@log = ();
ok WithState.parse('ab'), 'a grammar with state parses';
is @log.join(','), 'tweak:3', 'and builds it once';
nok WithState.parse('ab1'), 'and fails what its rules reject';

is OwnNew.new.mode, 'custom', "a grammar's own new still runs";
ok OwnNew.parse('42'), 'and its parse works';
