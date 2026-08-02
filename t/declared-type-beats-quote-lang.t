use Test;

plan 6;

# A declared symbol wins over the quoting language, as in raku. `Q` used to be
# lexed as a Q-quote delimited by `;`, which swallowed the following statement:
# `$c does Q; say "MARK";` parsed as `$c does Q(";\nsay \"MARK\"")`.
role Q { }
role Qw { }
class C { }

my @log;

my $c = C.new;
$c does Q;
@log.push('after-does');

is @log.elems, 1, 'the statement after `does Q;` is not swallowed';
ok $c ~~ Q, 'the role was mixed in';
is Q.^name, 'Q', 'a role named Q keeps its name';

# The same for the fused word-quote spellings.
sub takes(Qw $x) { 'typed' }
is takes(C.new does Qw), 'typed', 'a role named Qw is usable as a type';

# An *undeclared* quote word is still the quoting language. (`Q` and `Qw` are
# declared above, so `Q[...]` is now role parameterisation — in raku too.)
is q{plain}, 'plain', 'q{...} still quotes';
is qw<a b c>.join('-'), 'a-b-c', 'qw<> still word-quotes';
