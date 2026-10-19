use Test;

# A file-scope `my %h` / `my @a` that a named sub captures lives in a shared
# cell found by name. A block that declares its own `my %h` names another
# variable: an element store, a `:delete` or an increment through it must not
# reach the file-scope cell. `ev` names lexicals by string, which is what
# makes the file-scope variables captured. Checked against rakudo.

plan 6;

my %h = k => 'outer', n => 1;
my @a = 1, 2;
sub ev { EVAL q[%h] }
sub first-elem { @a[0] }

{
    my %h = k => 'in', n => 5;
    %h<new> = 1;
    %h<k>:delete;
    %h<n>++;
    my @a = 7;
    @a[0] = 9;
    is %h.sort.map(*.kv.join('=')).join(','), 'n=6,new=1', 'the block works on its own hash';
    is @a.join(','), '9', 'and on its own array';
}
is %h.sort.map(*.kv.join('=')).join(','), 'k=outer,n=1', 'the file-scope hash is untouched';
is ev().sort.map(*.kv.join('=')).join(','), 'k=outer,n=1', 'as the capturing sub sees it';
is first-elem(), 1, 'the file-scope array is untouched';

%h<top> = 2;
is ev()<top>, 2, 'a store at file scope still reaches the capturing sub';
