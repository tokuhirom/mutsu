use Test;

# ADR-11276 §9.111: the receiver-specific arms of `methods_0arg`'s prologue
# are rows, and the receivers they answered reach them.

plan 40;

# `Unicode` is a type object only (`DispatchShape::Unicode`).
isa-ok Unicode.version, Version, 'Unicode.version';
ok Unicode.version >= v15, 'Unicode.version names the shipped tables';
is Unicode.NFG, True, 'Unicode.NFG';
ok Unicode.^can('version'), 'Unicode.^can(version)';

# `List.from` and `List.to` (Rakudo declares them on `List`).
my @m = "abcd" ~~ m:g/<[bc]>/;
is @m.from, 1, 'a list of matches: .from is the first one\'s';
is @m.to, 3, 'a list of matches: .to is the last one\'s';
is [].to, Nil, 'an empty list: .to is Nil';
is ().from, Nil, 'an empty list: .from is Nil';
throws-like { (1, 2).to }, X::Method::NotFound, 'an element with no .to throws';
throws-like { @m.ast }, X::Method::NotFound, 'List has no .ast';

# A `Backtrace` answers `Any`'s `self.list.METHOD` methods on its frames.
sub frames { Backtrace.new }
my $bt = frames();
is $bt.elems, $bt.list.elems, 'Backtrace.elems counts the frames';
isa-ok $bt.List, List, 'Backtrace.List';
is $bt.List.elems, $bt.list.elems, 'Backtrace.List is the frames';
isa-ok $bt.Seq, Seq, 'Backtrace.Seq';
is $bt.Seq.elems, $bt.list.elems, 'Backtrace.Seq is the frames';
isa-ok $bt.Stringy, Str, 'Backtrace.Stringy';
is $bt.keys.elems, $bt.list.elems, 'Backtrace.keys indexes the frames';

# A compile-time diagnosis answers `line` and `filename` from its attributes;
# a typed one raised inside an EVAL names the EVAL unit too.
try EVAL "1 +* * 2 }";
is $!.^name, 'X::Syntax::Confused', 'a typed parse error';
is $!.line, 1, 'X::Syntax::Confused.line';
ok $!.filename.contains('EVAL'), 'X::Syntax::Confused.filename names the EVAL unit';

# A `Capture` answers `Any`'s `self.list.METHOD` methods on its positionals
# and `Hash` on its named part.
my $c = \(1, 2, :a(3));
is-deeply $c.Hash, {a => 3}, 'Capture.Hash';
isa-ok $c.Seq, Seq, 'Capture.Seq';
is $c.Seq, (1, 2), 'Capture.Seq is the positionals';
is $c.flat, (1, 2), 'Capture.flat';
is $c.WHAT, Capture, 'Capture.WHAT';
nok $c.is-lazy, 'Capture.is-lazy';
is $c.raku, '\(1, 2, :a(3))', 'Capture.raku';

# A user subclass of `Pair` or of a quant hash answers the built-in methods
# on its payload, and the coercion to its own base type is the instance.
class VP is Pair {}
my $vp = VP.new(key => "a", value => 1);
is $vp.key, 'a', 'Pair subclass: .key';
is $vp.antipair.raku, (1 => "a").raku, 'Pair subclass: .antipair';
is $vp.Pair.WHAT, VP, 'Pair subclass: .Pair is the instance';
is $vp.fmt("%s=%s"), 'a=1', 'Pair subclass: .fmt';
is $vp.invert.raku, '(1 => "a",).Seq', 'Pair subclass: .invert';
class MB is Bag {}
my $mb = MB.new(<a a b>);
is $mb.total, 3, 'Bag subclass: .total';
is $mb.kxxv.sort, <a a b>, 'Bag subclass: .kxxv';
is $mb.WHAT, MB, 'Bag subclass: .WHAT';
is $mb.elems, 2, 'Bag subclass: .elems';

# `True but False` is that Bool for every method (Rakudo).
my $tf = True but False;
is $tf.Int, 0, '(True but False).Int';
is $tf.not, True, '(True but False).not';
is $tf.Numeric, 0, '(True but False).Numeric';
is $tf.Str, 'False', '(True but False).Str';
