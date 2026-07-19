use v6;
use Test;

# A parenthesized assignment is TIGHT, so it is legal in a ternary branch:
#   cond ?? a !! ($x = y)
# mutsu used to wrap only `$`/`@`/`%`-named lvalues in the transparent `Grouped`
# node, so a method / attribute-accessor lvalue (`$.value = ...`, `$.value ~= ...`)
# fell through as a BARE AssignExpr and tripped the ternary "Assignment operators
# inside ?? !! are too loose" guard, even though the parens make it tight. Found
# via the real-dist compat sweep (Web::App's Web::Request::Multipart and
# RakuAST::Utils both hit it). See docs/dist-compat-sweep.md.

plan 5;

# --- simple assignment to an accessor lvalue in the else-branch ---
{
    my class C {
        has $.value is rw;
        method set-if($flag, $v) { $flag ?? 'skipped' !! ($.value = $v) }
    }
    my $c = C.new;
    $c.set-if(False, 42);
    is $c.value, 42, 'accessor `= ` in !! branch runs when cond is false';

    my $c2 = C.new(value => 7);
    $c2.set-if(True, 99);
    is $c2.value, 7, 'accessor `= ` in !! branch is skipped when cond is true';
}

# --- compound assignment (~=) to an accessor lvalue, multi-line (the dist shape) ---
{
    my class M {
        has $.value is rw = '';
        method feed($done, $line) {
            $done
              ?? 'done'
              !! ($.value ~= $line);
        }
    }
    my $m = M.new;
    $m.feed(False, 'a');
    $m.feed(False, 'b');
    is $m.value, 'ab', 'accessor `~=` in multi-line !! branch appends';
}

# --- accessor assignment in the then-branch too ---
{
    my class T {
        has $.value is rw = 0;
        method go($flag) { $flag ?? ($.value = 1) !! ($.value = 2) }
    }
    my $t = T.new;
    $t.go(True);
    is $t.value, 1, 'accessor `=` in ?? branch runs when cond is true';
}

# --- regression: parenthesized scalar stays a list-assignment target ---
{
    my $a;
    ($a) = 1, 2, 3;
    is $a.raku, '$(1, 2, 3)', 'parenthesized scalar still slurps the list (Grouped preserved)';
}
