use Test;

plan 10;

my $int = RakuAST::Type::Simple.new(RakuAST::Name.from-identifier("Int"));
my $str = RakuAST::Type::Simple.new(RakuAST::Name.from-identifier("Str"));

my $d = RakuAST::Type::Definedness.new(base-type => $int, definite => True);
is $d.raku, q:to/R/.chomp, 'Type::Definedness.new (definite) .raku';
RakuAST::Type::Definedness.new(
  base-type => RakuAST::Type::Simple.new(
    RakuAST::Name.from-identifier("Int")
  ),
  definite  => True
)
R
is $d.definite, True, '.definite';
is $d.base-type.raku, $int.raku, '.base-type';
is RakuAST::Type::Definedness.new(base-type => $int, definite => False).definite,
    False, '.definite of :U';

my $any = RakuAST::Type::AnyDefinedness.new(base-type => $int);
is $any.raku, q:to/R/.chomp, 'Type::AnyDefinedness.new .raku';
RakuAST::Type::AnyDefinedness.new(
  base-type => RakuAST::Type::Simple.new(
    RakuAST::Name.from-identifier("Int")
  )
)
R
is $any.base-type.raku, $int.raku, 'AnyDefinedness .base-type';

my $co = RakuAST::Type::Coercion.new(base-type => $int, constraint => $str);
is $co.raku, q:to/R/.chomp, 'Type::Coercion.new .raku';
RakuAST::Type::Coercion.new(
  base-type  => RakuAST::Type::Simple.new(
    RakuAST::Name.from-identifier("Int")
  ),
  constraint => RakuAST::Type::Simple.new(
    RakuAST::Name.from-identifier("Str")
  )
)
R
is $co.constraint.raku, $str.raku, 'Coercion .constraint';
is RakuAST::Type::Coercion.new(base-type => $int).base-type.raku, $int.raku,
    'a coercion without constraint keeps its base-type';
is EVAL($d).raku, 'Int:D', 'EVAL of a hand-built :D type';
