use Test;

# Methods the pure arity cascades answered by name, now rows of their owners
# (ADR-11276): each is reached from every call path, a variable receiver and a
# temporary alike.

plan 20;

# `Kernel`'s class methods on the type object.
ok Kernel.hostname.chars > 0, 'Kernel.hostname on the type object';
is Kernel.endian, $*KERNEL.endian, 'Kernel.endian agrees with $*KERNEL';
isa-ok Kernel.endian, Endian, 'Kernel.endian is an Endian';

# `Any.nl-out` on any receiver.
is 42.nl-out, "\n", 'Int.nl-out';
is Any.nl-out, "\n", 'Any.nl-out on the type object';
class Plain { }
is Plain.new.nl-out, "\n", 'a plain instance reaches Any.nl-out';

# `Thread.yield` is a class method.
is Thread.yield, Nil, 'Thread.yield';

# `Cool.AST` with `:compunit`.
isa-ok "1 + 2".AST, RakuAST::StatementList, 'Str.AST';
isa-ok "1 + 2".AST(:compunit), RakuAST::CompUnit, 'Str.AST(:compunit)';
isa-ok "1 + 2".AST(:!compunit), RakuAST::StatementList, 'Str.AST(:!compunit)';

# The two-argument rows.
is ("aaa bbb ccc " x 10).naive-word-wrapper(:max(20)).lines.elems, 6,
    'Str.naive-word-wrapper(:max)';
ok (1..10).in-range(5), 'Range.in-range($got)';
throws-like { (1..10).in-range(11, 'the value') }, X::OutOfRange,
    'Range.in-range($got, $what)';
ok "abcdef".substr-eq("cd", 2), 'Str.substr-eq($needle, $pos)';
is-deeply "a,b,c".split(",", 2), ("a", "b,c").Seq, 'Str.split($sep, $limit)';
is-deeply "abcdef".comb(2, 2), ("ab", "cd").Seq, 'Str.comb($size, $limit)';

# `Exception.resume` signals the handler.
my $resumed = 0;
{
    warn "w";
    $resumed++;
    CONTROL { when CX::Warn { .resume } }
}
is $resumed, 1, 'resuming a warning continues after it';

# `Pod::Block::Declarator`'s accumulators.
my $pod = Pod::Block::Declarator.new;
$pod._add_leading("first");
$pod._add_leading("second");
is $pod.leading, 'first second', '_add_leading space-joins the pieces';

# A RakuAST builder changes the node in place.
my $list = RakuAST::StatementList.new;
$list.add-statement(RakuAST::Statement::Expression.new(
    expression => RakuAST::IntLiteral.new(42)));
is $list.statements.elems, 1, 'RakuAST::StatementList.add-statement';

# `Backtrace::Frame` has no `name` (Rakudo's has `subname`).
sub f { Backtrace.new.list.first(*.subname eq 'f') }
nok f().^can('name') && f().name.defined && f().name ne f().subname,
    'no name alias on a frame';
