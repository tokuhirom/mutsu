use Test;

# Raku's custom-container protocol: `my @v is TypeName = ...` binds the variable
# to `TypeName.new` (called with NO arguments) and routes the declaration's own
# initializer through the class's `STORE`, with `:INITIALIZE` set. A later
# `@v = ...` goes through `STORE` too, without `:INITIALIZE`. Reads and
# coercions then dispatch to the bound instance's own methods.
#
# Every row below was measured against rakudo first. The `@`-sigil half of this
# was previously inert: the tie only engaged for classes that inherited `Array`,
# composed `Positional`, or defined `AT-POS`, and even then it called
# `TypeName.new(|@values)` — the wrong protocol — so the documented
# `Language/subscripts.rakudoc` DNA example never reached `STORE` at all.

plan 7;

subtest 'the documented DNA custom container (Language/subscripts.rakudoc)' => {
    plan 4;

    my @log;

    class DNA {
        has $.chain;
        method STORE(Str $chain where { $chain ~~ /^^ <[ACGT]>+ $$/ and $chain.chars %% 3 },
                     :$INITIALIZE --> DNA) {
            @log.push($INITIALIZE ?? "init:$chain" !! "store:$chain");
            $!chain := $chain;
            self
        }
        method Str(::?CLASS:D:) { $!chain.comb.rotor(3).map(*.join('')).join('|') }
    }

    my @string is DNA = 'GAATCC';
    is ~@string, 'GAA|TCC', 'declaration STORE ran and .Str dispatches to the class';

    @string = 'ACGTCG';
    is ~@string, 'ACG|TCG', 'reassignment routes through STORE too';

    is-deeply @log, ['init:GAATCC', 'store:ACGTCG'],
        'STORE saw :INITIALIZE on the declaration only';

    # The signature is `Str $chain`, so a 1-element List would fail its type
    # check: raku hands STORE the RHS as written, not wrapped in a list.
    lives-ok { @string = 'GGGCCC' }, 'a scalar RHS reaches a typed single-positional STORE unwrapped';
}

subtest 'STORE receives the RHS shaped the way raku shapes it' => {
    plan 6;

    my @seen;
    class Shape { method STORE(|c) { @seen.push(c.list[0]); self } }

    my @a is Shape = 'x';
    is-deeply @seen.pop, 'x', 'a bare scalar initializer stays a scalar';

    my @b is Shape = 'x', 'y';
    is-deeply @seen.pop.list.Array, ['x', 'y'], 'a comma list arrives as one list';

    my @src = 1, 2;
    my @c is Shape = @src;
    is-deeply @seen.pop.list.Array, [1, 2], 'an array initializer arrives as one list';

    @a = 'z';
    is-deeply @seen.pop, 'z', 'a bare scalar reassignment stays a scalar';

    @a = 'z', 'w';
    is-deeply @seen.pop.list.Array, ['z', 'w'], 'a comma list reassignment arrives as one list';

    my @d is Shape;
    is-deeply @seen, [], 'a declaration with no initializer never calls STORE';
}

subtest 'the tie engages for a plain class with only STORE' => {
    plan 3;

    class Bare { has $.v; method STORE($x) { $!v = $x; self }; method Str { "Bare<$!v>" } }

    my @a is Bare = 'q';
    is @a.^name, 'Bare', '.^name reports the tied class, not Array';
    is ~@a, 'Bare<q>', 'stringification dispatches to the class';
    is @a.VAR.^name, 'Bare', '.VAR reports the tied class: a tied variable IS its own container';
}

subtest 'element reads and writes dispatch to the class' => {
    plan 3;

    my @posted;
    class Vec {
        has @.items;
        method STORE(|c) { @!items = c.list[0].list; self }
        method AT-POS($i) { @!items[$i] }
        method ASSIGN-POS($i, $v) { @posted.push("$i=$v"); @!items[$i] = $v }
        method Str { '<' ~ @!items.join(',') ~ '>' }
    }

    my @v is Vec = 1, 2, 3;
    is @v[1], 2, 'AT-POS serves an element read';
    @v[1] = 9;
    is-deeply @posted, ['1=9'], 'ASSIGN-POS serves an element write';
    is @v[1], 9, 'the written element reads back through AT-POS';
}

subtest 'the %-sigil tie keeps working and gets the same argument shape' => {
    plan 4;

    my @seen;
    class Tk does Associative {
        has %.s;
        method STORE(|c) { @seen.push(c.hash<INITIALIZE>.so); for c.list[0].list -> $p { %!s{$p.key} = $p.value }; self }
        method AT-KEY($k) { %!s{$k} }
    }

    my %h is Tk = a => 1, b => 2;
    is %h<a>, 1, 'declaration initializer stored through STORE';
    is-deeply @seen, [True], 'declaration STORE saw :INITIALIZE';

    %h = c => 3;
    is %h<c>, 3, 'reassignment stored through STORE';
    is-deeply @seen, [True, False], 'reassignment STORE did NOT see :INITIALIZE';
}

subtest 'a class inheriting a native container still uses the constructor path' => {
    plan 2;

    # `class A is Array[Str] {}` has no STORE of its own; it is constructed from
    # the initializer values, NOT via the STORE protocol. Pinned so the
    # STORE-protocol branch cannot swallow this shape (roast
    # S05-grammar/inheritance.t depends on it).
    my class A is Array[Str] { }
    my @a is A = <a b c>;
    is @a, 'a b c', 'the array initialized ok';
    is-deeply @a.of, Str, 'and kept its element type';
}

subtest 'a role names a tie just as well as a class' => {
    plan 2;

    my @seen;
    role Rolly { method STORE(|c) { @seen.push(c.list[0]); self }; method Str { 'rolly' } }

    my @r is Rolly = 'k';
    is ~@r, 'rolly', 'a punned role ties the variable';
    is-deeply @seen, ['k'], 'and its STORE saw the initializer';
}
