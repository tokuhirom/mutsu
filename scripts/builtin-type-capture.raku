#!/usr/bin/env raku
# Regenerates src/runtime/builtin_type_capture.tsv: what `T.Capture` does in
# rakudo for each core class `T` called as a type object (#18778).
#
#   raku scripts/builtin-type-capture.raku > src/runtime/builtin_type_capture.tsv
#
# One row per class: `Name<TAB>kind`, where `kind` is
#   empty      -- `Mu.Capture` over a class with no attributes: `\()`
#   attributes -- `Mu.Capture` over a class with attributes: dies
#                 "Cannot look up attributes in a T type object. Did you
#                 forget a '.new'?"
#   instance   -- the class's own `Capture` is `:D`: dies "Invocant of method
#                 'Capture' must be an object instance of type 'T', not a type
#                 object of type 'T'.  Did you forget a '.new'?"
# A class whose type object answers anything else (`Int.Capture` dies with
# X::Cannot::Capture, `Seq.Capture` is `\(Seq)`) is not listed: mutsu answers
# those from the class's own rows.

my %seen;
my @rows;
sub walk($stash, Str $prefix) {
    for $stash.keys.sort -> $key {
        next unless $key ~~ /^ <[A..Z]>/;
        my $name = $prefix ~ $key;
        next if %seen{$name}++;
        my \type = $stash{$key};
        next unless type.^name eq $name;
        classify(type, $name) if type.HOW ~~ Metamodel::ClassHOW;
        walk(type.WHO, $name ~ '::') if (try type.WHO.keys);
    }
}
sub classify(Mu \type, $name) {
    my $kind;
    try {
        my $c = type.Capture;
        $kind = 'empty' if $c ~~ Capture && !$c.list && !$c.hash;
        CATCH {
            default {
                my $m = .message;
                $kind = $m.starts-with('Cannot look up attributes in a') ?? 'attributes'
                  !! $m.starts-with("Invocant of method 'Capture' must be an object instance") ?? 'instance'
                  !! Nil;
            }
        }
    }
    @rows.push: "$name\t$kind" if $kind;
}

walk(CORE::, '');
.say for @rows.sort;
