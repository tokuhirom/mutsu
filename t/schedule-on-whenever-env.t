use Test;

# A supply tapped through `.schedule-on($scheduler)` used to run the
# `whenever` body WITHOUT the supply block's lexicals in env: a `my enum`
# declared in the supply block was invisible there, so a bareword enum value
# died with `X::Undeclared::Symbols`. Reproducing this needs a module that
# makes the enum's member name a suppressed name too (t/suppressed-type-vs-
# local-decl-lib/SuppMod.rakumod) -- without a collision the bareword still
# resolved via the type registry, masking the env loss. This blocked
# Cro::HTTP::ResponseParser's `transformer(...)` helper, which taps through
# `.schedule-on($*SCHEDULER)` and declares `my enum Expecting <StatusLine
# Header Body>` while Cro::HTTP::Header has a lexical `my grammar Header`.

use lib $*PROGRAM.parent.add('suppressed-type-vs-local-decl-lib').Str;
use SuppMod;

plan 1;

my $scheduler = CurrentThreadScheduler.new;
my $in = Supplier.new;
my $out = supply {
    my enum E <A Header B>;
    whenever $in -> $v { emit Header.Int }
};
my @got;
$out.schedule-on($scheduler).tap: -> $x { @got.push($x) },
    quit => -> $ex { @got.push("QUIT: {$ex.gist}") };
$in.emit(1);
$in.done;

is @got.join(","), "1",
    "a whenever body tapped through schedule-on still sees the supply block's my enum";
