use Test;

# `RakuAST::Name.canonicalize` of a stash name keeps its trailing `::`; the
# bare `::` spells as the empty string. A CHECK-time tree walk that asked every
# `Call::Name` for its name died on `EXPORT::DEFAULT::.AT-KEY(...)`.

plan 3;

sub callee-name(Str $src) {
    $src.AST.statements[0].expression.operand.name.canonicalize
}

is callee-name(Q|EXPORT::DEFAULT::.AT-KEY("x")|), 'EXPORT::DEFAULT::', 'qualified stash name';
is callee-name(Q|Foo::.keys|), 'Foo::', 'one-part stash name';
is callee-name(Q|::.keys|), '', 'the bare `::` is empty';
