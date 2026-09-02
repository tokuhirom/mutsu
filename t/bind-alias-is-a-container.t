use Test;

# A `:=` bind aliases the SOURCE CONTAINER, and the alias keeps working from
# every frame — including a closure that is stored and called later.
#
# Two independent defects used to break this:
#
#  1. A same-scope DECLARATION bind of a plain scalar (`my $t := $s`,
#     `my \x := $s`) recorded only a frame-local slot pair instead of promoting
#     the pair to a shared container cell. Inside a stored closure the alias
#     then read a value frozen at capture time and wrote to a by-name copy, so
#     the source never changed. The `$t := $s` REBIND spelling always worked,
#     which is what pinned the diagnosis.
#  2. A sigilless bind decided mutability from the SYNTAX of its right-hand
#     side (`matches!(expr, Expr::Var(_))`), so every other lvalue shape —
#     `@a[0]`, `%h<k>`, an `is rw` accessor call, a computed index — was marked
#     readonly and refused the write. Raku decides from what the name is bound
#     to, at run time.

plan 34;

# --- 1. the alias is live in both directions, in the declaring frame ---------
{
    my $s = "a";
    my $t := $s;
    $t = 42;
    is $s, 42, 'write through a $-sigil decl-bind alias reaches the source';
}
{
    my $s = "a";
    my $t := $s;
    $s = "z";
    is $t, "z", 'a write to the source is visible through the alias';
}
{
    my $s = "a";
    my \x := $s;
    x = 42;
    is $s, 42, 'write through a sigilless decl-bind alias reaches the source';
}

# --- 2. ... and from inside a STORED closure --------------------------------
{
    my $s = "a";
    my $t := $s;
    my $f = { $t = 42 };
    $f();
    is $s, 42, 'a stored closure writing the alias reaches the source';
}
{
    my $s = "a";
    my $t := $s;
    my $f = { $t };
    $s = "z";
    is $f(), "z", 'a stored closure reads the alias live, not a capture snapshot';
}
{
    my $s = "a";
    my \x := $s;
    my $f = sub { x = 42 };
    $f();
    is $s, 42, 'an anon sub writing a sigilless alias reaches the source';
}
{
    my $s = "a";
    my \x := $s;
    sub named-writer { x = 42 }
    named-writer();
    is $s, 42, 'a named sub writing a sigilless alias reaches the source';
}
{
    my $s = "a";
    my \x := $s;
    sub named-reader { x }
    $s = "z";
    is named-reader(), "z", 'a named sub reads a sigilless alias live';
}
{
    my $s = "a";
    my $t := $s;
    my $f = { my $g = { $t = 42 }; $g() };
    $f();
    is $s, 42, 'the alias survives two nested closure boundaries';
}
{
    # The alias and the source are ONE container: a write through either is
    # visible through the other afterwards.
    my $s = "a";
    my $t := $s;
    my $f = { $t = 42 };
    $f();
    is $t, 42, 'the alias itself reads back the written value';
    is $s, 42, 'and so does the source';
}

# --- 3. the type constraint travels with the container ----------------------
{
    my Int $a = 5;
    my \x := $a;
    my &blk = sub { x = "not an int" };
    dies-ok { blk() }, 'a bad write through a captured alias is type-checked';
    is $a, 5, 'and the source is left untouched';
}
{
    my Int $a = 5;
    my \x := $a;
    my $err;
    { x = "nope"; CATCH { default { $err = .message } } }
    like $err, /'expected Int but got Str'/, 'the type-check message names the types';
    like $err, /'assignment to $a'/, 'and names the variable the container came from';
}
{
    my Int $a = 5;
    my \x := $a;
    x = 7;
    is $a, 7, 'a well-typed write through the alias still goes through';
}

# --- 4. a sigilless bind to any LVALUE shape aliases its container -----------
{
    my @a = 1, 2;
    my \x := @a[0];
    x = 9;
    is-deeply @a, [9, 2], 'sigilless bind to an array element writes through';
}
{
    my @a = 1, 2;
    my \x := @a[1 + 0];
    x = 9;
    is-deeply @a, [1, 9], 'a computed index binds the element container too';
}
{
    my %h = a => 1;
    my \x := %h<a>;
    x = 9;
    is %h<a>, 9, 'sigilless bind to a hash element writes through';
}
{
    my @a = 1, 2;
    my \x = @a[0];
    x = 9;
    is-deeply @a, [9, 2], 'the `=` spelling of a sigilless decl binds as well';
}
{
    class RwHolder { has $.v is rw }
    my $c = RwHolder.new(v => 1);
    my \x := $c.v;
    x = 9;
    is $c.v, 9, 'sigilless bind to an `is rw` accessor writes through';
}
{
    my @a = 1, 2;
    my \x := @a[0];
    my $f = { x = 9 };
    $f();
    is-deeply @a, [9, 2], 'an element alias survives into a stored closure';
}
{
    my @a = 1, 2;
    my \x := @a;
    x.push(3);
    is-deeply @a, [1, 2, 3], 'sigilless bind to a whole array shares it';
}
{
    # An object hash stores `.WHICH`-encoded keys; the bind subscript has to
    # encode its key the same way or it misses the entry and yields `Any`.
    # This was broken for the `$`-sigil spelling too, long before the sigilless
    # one was routed onto the same path.
    my %h{Any};
    %h{Int} = 1;
    my $t := %h{Int};
    is $t, 1, 'a `:=` bind to an object-hash element finds the entry';
    $t = 9;
    is %h{Int}, 9, 'and writes through to it';
    my \u := %h{Int};
    is u, 9, 'the sigilless spelling finds it too';
}

# --- 5. ... and a bind to a VALUE stays immutable ---------------------------
{
    my \lit-bound := 5;
    dies-ok { lit-bound = 9 }, 'a sigilless term bound to a literal is immutable';
}
{
    my \eq-bound = 5;
    dies-ok { eq-bound = 9 }, 'the `=` spelling of a value bind is immutable too';
}
{
    my $s = "a";
    my \call-bound := $s.uc;
    dies-ok { call-bound = 9 }, 'a sigilless term bound to a method RESULT is immutable';
}
{
    my \msg-bound := 5;
    my $err;
    { msg-bound = 9; CATCH { default { $err = .message } } }
    like $err, /'Cannot modify an immutable Int'/, 'and it names the immutable value';
}

# --- 6. shapes that already worked, kept as controls ------------------------
{
    my $s = "a";
    my $t;
    $t := $s;
    my $f = { $t = 42 };
    $f();
    is $s, 42, 'control: the rebind spelling still aliases';
}
{
    my $s = "a";
    sub takes-raw(\p) { p = 42 }
    takes-raw($s);
    is $s, 42, 'control: a sigilless PARAMETER still aliases its argument';
}
{
    # Shadow safety: the bind must reach the visible declaration, not an
    # unrelated same-named one from a sibling block.
    my $v = 10;
    { my $v = 99; }
    my \x := $v;
    x = 1;
    is $v, 1, 'control: the bind reaches the visible declaration';
}
{
    # A bind to the topic must capture the topic's current referent, never
    # promote `$_` itself into a shared cell.
    my $seen;
    for 1, 2 -> $n {
        $_ = $n;
        my $ex := $_;
        $seen = $ex;
    }
    is $seen, 2, 'control: binding the topic does not corrupt it';
}
