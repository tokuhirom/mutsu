use Test;
use experimental :rakuast;

# A user candidate of a core prefix operator (`multi prefix:<!>`, as Red
# exports) leaves `!$x` the operator application `ApplyPrefix(Prefix('!'))` in
# the unit tree, not a call of `prefix:<!>`; Red's compiler reads it as an
# operator (#17501). The candidate still applies.

plan 6;

class A { }
multi prefix:<!>(A $a) { 'user' }

my @seen;
CHECK {
    my @s = $*CU.statement-list.statements;
    sub decl($name) {
        @s.first({ .^name eq 'RakuAST::Statement::Expression'
            && .expression.^name eq 'RakuAST::VarDeclaration::Simple'
            && .expression.name eq $name }).expression
    }
    my $not = decl('$applied');
    @seen.push: $not.initializer.expression.^name;
    @seen.push: $not.initializer.expression.prefix.operator;
    my $call = decl('$called');
    @seen.push: $call.initializer.expression.^name;
}

my $applied = !A.new;
my $called = prefix:<!>(A.new);

is @seen[0], 'RakuAST::ApplyPrefix', 'the operator form is an ApplyPrefix';
is @seen[1], '!', 'of the ! prefix';
is @seen[2], 'RakuAST::Call::Name', 'the call form stays a call';
is $applied, 'user', 'the operator form calls the user candidate';
is $called, 'user', 'so does the call form';
is !0, True, 'the core candidate stays';
