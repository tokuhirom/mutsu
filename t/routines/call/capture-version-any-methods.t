use Test;

plan 9;

# #13477: Any's reverse/invert/fmt reach Capture and Version.
is \(1, 2, a => 3).reverse.raku, '(2, 1).Seq', 'Capture.reverse reads the positionals';
is (v1.2).reverse.raku, '(v1.2,).Seq', 'Version.reverse is a one-element list';
is \(1 => 2, 3 => 4).invert.raku, '(2 => 1, 4 => 3).Seq', 'Capture.invert inverts its pairs';
throws-like { \(1, 2).invert }, X::TypeCheck, 'Capture.invert of non-pairs';
throws-like { (v1.2).invert }, X::TypeCheck, 'Version.invert';
throws-like { \(1, 2).fmt }, X::Multi::NoMatch, 'Capture.fmt has no candidate';
throws-like { \(1, 2).fmt("%s") }, X::Multi::NoMatch, 'Capture.fmt with a format';
throws-like { (v1.2).fmt }, X::Multi::NoMatch, 'Version.fmt has no candidate';
throws-like { (v1.2).fmt("%s") }, X::Multi::NoMatch, 'Version.fmt with a format';
