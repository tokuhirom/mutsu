use Test;

# `RakuAST::Doc::Block` (#14880): the constructor, `.raku` and the accessors,
# as rakudo 2026.09 answers them. Reading a Pod block out of source is a later
# slice.

plan 14;

use experimental :rakuast;

sub flat(Str $s) { $s.subst(/\s+/, ' ', :g) }

my $b = RakuAST::Doc::Block.new(type => "pod", paragraphs => ("hello\n",));
is flat($b.raku), 'RakuAST::Doc::Block.new( type => "pod", paragraphs => ( "hello\n", ) )',
    'a delimited block prints type and paragraphs';
is $b.type, 'pod', 'type';
is $b.level.raku, '""', 'a missing level is the empty string';
is $b.delimited, True, 'a block that is neither a directive, abbreviated nor extended is delimited';
is ($b.for, $b.abbreviated, $b.directive).join(','), 'False,False,False', 'the flags are false';
is $b.paragraphs.raku, '("hello\n",)', 'paragraphs';
is $b.config.raku, 'Map.new', 'an empty config is an empty Map';

my $h = RakuAST::Doc::Block.new(
    type => "head", level => 2, abbreviated => True, paragraphs => ("Foo\n",),
    config => %(:a(RakuAST::StrLiteral.new("b"))),
);
is flat($h.raku),
    'RakuAST::Doc::Block.new( type => "head", level => 2, abbreviated => True, '
    ~ 'config => ( :a(RakuAST::StrLiteral.new("b")), ), paragraphs => ( "Foo\n", ) )',
    'level, abbreviated flag and config print in rakudo order';
is $h.level.raku, '"2"', 'a level is answered as a string';
is $h.delimited, False, 'an abbreviated block is not delimited';
is $h.config.keys.join(','), 'a', 'config answers a Map over the pairs';

is flat(RakuAST::Doc::Block.new(type => "x", for => True, abbreviated => True).raku),
    'RakuAST::Doc::Block.new( type => "x", abbreviated => True )',
    'abbreviated beats for';
is flat(RakuAST::Doc::Block.new(type => "x", directive => True, abbreviated => True).raku),
    'RakuAST::Doc::Block.new( type => "x", directive => True )',
    'directive beats abbreviated';
dies-ok { RakuAST::Doc::Block.new() }, 'type is required';
