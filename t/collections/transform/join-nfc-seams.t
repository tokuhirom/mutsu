use Test;
use nqp;

# `.join` builds an NFC string: a combining mark at the start of a piece (or
# of the separator) composes with the end of the piece before it, as `~`
# composes across its seam (#17889).

plan 10;

my @p = "a", "\x[301]";
ok @p.join eq "\x[e1]", 'Array.join composes across a seam';
ok ("a", "\x[301]").join eq "\x[e1]", 'List.join';
ok ("a", "\x[301]").Seq.join eq "\x[e1]", 'Seq.join';
ok join("", "a", "\x[301]") eq "\x[e1]", 'the join routine';
ok ("a", "b").join("\x[301]") eq "\x[e1]b", 'a combining separator composes with the piece before it';
ok (^2).map({ $_ ?? "\x[301]" !! "a" }).join eq "\x[e1]", 'a lazy map joined';
ok \("a", "\x[301]").join eq "\x[e1]", 'Capture.join';
ok nqp::join("", nqp::list_s("a", "\x[301]")) eq "\x[e1]", 'nqp::join';
is ("a", "\x[301]").join.encode.decode, ("a", "\x[301]").join, 'a joined string round-trips through the encoder';
is <x y z>.join(","), "x,y,z", 'an ASCII join is unchanged';
