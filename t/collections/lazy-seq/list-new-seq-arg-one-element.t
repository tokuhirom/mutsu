use Test;

plan 8;

is List.new((1,2).Seq).elems, 1, 'a Seq argument stays one element';
is List.new((1,2).Seq).raku, '((1, 2).Seq,)', '.raku of List.new(Seq)';
is List.new((1,2).map(*+0)).elems, 1, 'a map Seq stays one element';
is List.new(gather { take 1; take 2 }).elems, 1, 'a gather stays one element';
is List.new(1..3).elems, 1, 'a Range argument stays one element';
is List.new((1,2).Seq, 3).elems, 2, 'with a second argument too';
is List.new((1,2)).raku, '((1, 2),)', 'a List argument stays whole (unchanged)';
is Array.new((1,2).Seq).raku, '[1, 2]', 'Array.new still spreads its single Seq';
