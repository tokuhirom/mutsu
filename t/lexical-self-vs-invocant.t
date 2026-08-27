use v6;
use Test;

# `self` is a *term* in Raku, not a `$`-sigiled variable, so a user's `my $self`
# is an ordinary lexical that never interacts with a method's invocant. mutsu
# stores scalars sigil-less, which used to put both on the env key `self`: a
# `$self` captured by a closure was then silently replaced by whatever invocant
# the closure was next called with, and the `Proxy` form of that recursed into
# `FETCH` until the process died. ADR-0061 gives the lexical its own key.

plan 29;

class Outer { method tag { 'OUTER' } }
class Inner { method tag { 'INNER' } }

# --- 1. A mainline `my $self` is visible inside a method body, and is not the
#        invocant.
{
    my $self = 'mainline';
    class C1 { method m { $self } }
    is C1.new.m, 'mainline', 'a mainline `my $self` reaches a method body unchanged';
}

# --- 2. `self` inside a method is still the invocant, never a same-named outer
#        lexical (the negative direction).
{
    my $self = 'mainline';
    class C2 { method m { self.^name } }
    is C2.new.m, 'C2', '`self` in a method body is the invocant, not an outer $self';
}

# --- 3. A `$self` captured by a method literal survives being called with a
#        different invocant.
{
    my $self = Outer.new;
    my $m = method () { $self.tag };
    is $m(Inner.new), 'OUTER', 'a captured $self is not replaced by the callee invocant';
}

# --- 4. The `make-cb` shape: a method-local `my $self = self` captured by a
#        method literal it returns.
{
    class C4 {
        method make-cb { my $self = self; method () { $self.^name } }
    }
    is C4.new.make-cb()(Inner.new), 'C4',
        'a method-local `my $self = self` is captured, not re-bound by the callee';
}

# --- 4b. ... and still so when an unrelated mainline `$self` is in scope.
{
    my $self = 'mainline';
    class C4b {
        method make-cb { my $self = self; method () { $self.^name } }
    }
    is C4b.new.make-cb()(Inner.new), 'C4b',
        'the method-local $self wins over a same-named mainline lexical';
}

# --- 5. The damaging form: `my $self = self` + `Proxy`, the standard way to
#        write an `is rw` AT-POS. This used to overflow the stack, because the
#        substituted invocant was the Proxy itself and reading it re-entered
#        FETCH forever.
{
    class C5 {
        has @.nodes;
        method AT-POS($offset) is rw {
            my $self = self;
            Proxy.new(
                FETCH => method () { $self.nodes[$offset] },
                STORE => method ($val) { $self.nodes[$offset] = $val },
            )
        }
    }
    my $doc = C5.new(nodes => ['x', 'y']);
    is $doc[1], 'y', 'a `my $self` + Proxy AT-POS reads through FETCH';
    is $doc[0], 'x', 'and again for another index';
}

# --- 5b. The same for AT-KEY.
{
    class C5b {
        has %.attribs;
        method AT-KEY($key) is rw {
            my $self = self;
            Proxy.new(
                FETCH => method () { $self.attribs{$key} },
                STORE => method ($val) { $self.attribs{$key} = $val },
            )
        }
    }
    my $doc = C5b.new(attribs => { a => 1 });
    is $doc<a>, 1, 'a `my $self` + Proxy AT-KEY reads through FETCH';
}

# --- 6. NEGATIVE: an explicit invocant parameter genuinely named `self`.
{
    class C6 { method bar($self: $n) { $self.^name ~ ':' ~ $n } }
    is C6.new.bar(7), 'C6:7', '`method bar($self: $n)` binds $self to the invocant';
}

# --- 6b. ... including in a method literal.
{
    my $m = method ($self: $n) { $self.^name ~ '/' ~ $n };
    is $m(Inner.new, 3), 'Inner/3', 'a method literal with an explicit $self: invocant';
}

# --- 6c. An anonymous invocant marker declares nothing, so an outer `$self`
#         still shows through.
{
    my $self = 'mainline';
    class C6c { method m(C6c:D:) { $self } }
    is C6c.new.m, 'mainline', 'an anonymous invocant marker does not declare $self';
}

# --- 7. An ordinary (non-invocant) `$self` parameter.
{
    my $self = 'mainline';
    my $f = sub ($self) { "sub:$self" };
    is $f('A'), 'sub:A', 'a `sub ($self)` parameter shadows an outer $self';
    my $b = -> $self, $x { "blk:$self$x" };
    is $b('B', 'C'), 'blk:BC', 'a `-> $self, $x` block parameter does too';
}

# --- 7b. A `$self` parameter stays visible inside a nested block.
{
    my $f = sub ($self) { my $inner = { $self ~ '!' }; $inner() };
    is $f('P'), 'P!', 'a $self parameter is visible from a nested closure';
}

# --- 8. Assignment: a plain `my $self` is assignable anywhere ...
{
    my $self;
    $self = 42;
    is $self, 42, 'assignment to a `my $self` works at top level';
    class C8 { method m { my $self = 1; $self = 2; $self } }
    is C8.new.m, 2, 'and to a method-local `my $self` too';
}

# --- 8b. ... while the invocant term stays immutable inside a method.
{
    class C8b { method f { self = 5 } }
    dies-ok { C8b.new.f }, '`self = ...` inside a method still dies';
}

# --- 9. Interpolation and method calls on a lexical $self.
{
    my $self = Outer.new;
    class C9 { method m { "{$self.tag}" } }
    is C9.new.m, 'OUTER', 'a lexical $self interpolates and dispatches normally';
}

# --- 10. The other spellings that turn a `$self` token into a name: string
#         interpolation, a `:$self` colonpair, and the read-modify-write /
#         binding forms.
{
    my $self = 'OUT';
    is "interp: $self", 'interp: OUT', 'a lexical $self interpolates directly';
    is "call: $self.lc()", 'call: out', 'and with a trailing method call';
}
{
    sub named(:$self = 'default') { $self }
    is named(), 'default', 'a `:$self` named parameter defaults';
    is named(self => 'given'), 'given', 'and binds its argument';
}
{
    my $outer = 'OUT';
    class C10 {
        method m {
            my $self = 5;
            $self++;
            $self += 2;
            my $r := $self;
            $r = 9;
            $self
        }
    }
    is C10.new.m, 9, '$self supports ++, += and := like any other lexical';
    is $outer, 'OUT', 'and none of that disturbed the enclosing scope';
}

# --- 10b. A `$self` parameter must also reach a body run through the AST
#          *carrier* path, which recompiles `SubData::body` with a bare compiler
#          and so cannot consult `Compiler::self_is_signature_param`. `Date`'s
#          formatter callback is a real instance of that path.
{
    my $us = sub ($self) { sprintf "%02d/%02d/%04d", .month, .day, .year given $self };
    is Date.new('2015-12-31', formatter => $us).Str, '12/31/2015',
        'a `sub ($self)` invoked through a native callback binds $self';
}

# --- 10c. The remaining shapes where the name `self` arrives from somewhere
#          other than `my $self`: a SINGLE pointy-block parameter (which the
#          legacy binding path carries as a bare name with no ParamDef at all),
#          a destructuring sub-signature, and a `$self` parameter captured by a
#          closure that outlives its frame.
{
    my $b1 = -> $self { "one:$self" };
    is $b1('B'), 'one:B', 'a single `-> $self` pointy parameter binds $self';

    sub destr([$self, $x]) { "destr:$self/$x" }
    is destr([1, 2]), 'destr:1/2', 'a destructured sub-signature `$self` binds too';

    sub outer-cb($self) { my $cb = { "cap:$self" }; $cb }
    is outer-cb('Z')(), 'cap:Z', 'a $self parameter survives into an escaping closure';
}

# --- 11. A `for` loop parameter spelled `$self`.
{
    my $self = 'OUT';
    my $acc = '';
    for 1, 2 -> $self { $acc ~= $self }
    is $acc ~ '/' ~ $self, '12/OUT',
        'a `for ... -> $self` parameter shadows an outer $self only inside the loop';
}

done-testing;
