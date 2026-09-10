use Test;

# Element-level mutations (push/pop/shift/unshift/splice/append/prepend,
# element/slice assignment, hash key writes, :delete, ++/--) mutate the
# variable's container IN PLACE, so every by-value holder of the same
# container — an `@a` captured into a list `(0, @a)`, an element, a scalar —
# observes the mutation (Raku container identity, BLOCKERS §3.2 item 1).
# Copies (`my @b = @a`, `is copy` params, `.clone`) stay detached: `=` copy
# semantics are enforced at copy time, not by copy-on-write at mutation time.

plan 60;

# --- array method mutations visible through a by-value capture ---
{ my @a = 1,2;   my $c = (0, @a); @a.push(9);        is $c[1].join(','), '1,2,9',   'push visible through capture'; }
{ my @a = 1,2;   my $c = (0, @a); @a.pop;            is $c[1].join(','), '1',       'pop visible through capture'; }
{ my @a = 1,2;   my $c = (0, @a); @a.shift;          is $c[1].join(','), '2',       'shift visible through capture'; }
{ my @a = 1,2;   my $c = (0, @a); @a.unshift(0);     is $c[1].join(','), '0,1,2',   'unshift visible through capture'; }
{ my @a = 1,2,3; my $c = (0, @a); @a.splice(1,1);    is $c[1].join(','), '1,3',     'splice visible through capture'; }
{ my @a = 1,2;   my $c = (0, @a); @a.append(3,4);    is $c[1].join(','), '1,2,3,4', 'append visible through capture'; }
{ my @a = 1,2;   my $c = (0, @a); @a.prepend(0);     is $c[1].join(','), '0,1,2',   'prepend visible through capture'; }
{ my @a = 1,2;   my $c = (0, @a); @a.push(8, 9);     is $c[1].join(','), '1,2,8,9', 'multi-arg push visible through capture'; }
{ my @a = 1,2,3; my $c = (0, @a); @a.splice(1,1,<x y>); is $c[1].join(','), '1,x,y,3', 'splice with replacement visible'; }

# --- element / slice writes ---
{ my @a = 1,2;   my $c = (0, @a); @a[0] = 9;         is $c[1].join(','), '9,2',     'element assign visible'; }
{ my @a = 1,2;   my $c = (0, @a); @a[2] = 3;         is $c[1].join(','), '1,2,3',   'element extend visible'; }
{ my @a = 1,2;   my $c = (0, @a); @a[0]++;           is $c[1].join(','), '2,2',     'element increment visible'; }
{ my @a = 1,2,3; my $c = (0, @a); @a[0,1] = 8,9;     is $c[1].join(','), '8,9,3',   'slice assign visible'; }
{ my @a = 1,2;   my $c = (0, @a); @a[*-1] = 9;       is $c[1].join(','), '1,9',     'whatever-index assign visible'; }
{ my @a = 1,2;   my $c = (0, @a); @a = 7,8;          is $c[1].join(','), '7,8',     'whole-array reassign visible (identity)'; }

# --- hash writes ---
{ my %h = a=>1;      my $c = (0, %h); %h<b> = 2;     is $c[1].keys.sort.join(','), 'a,b', 'hash key assign visible'; }
{ my %h = a=>1,b=>2; my $c = (0, %h); %h<a>:delete;  is $c[1].keys.sort.join(','), 'b',   'hash key delete visible'; }
{ my %h = a=>1;      my $c = (0, %h); %h<a>++;       is $c[1]<a>, 2,                      'hash key increment visible'; }
{ my %h = a=>1;      my $c = (0, %h); %h = c=>3;     is $c[1].keys.sort.join(','), 'c',   'whole-hash reassign visible (identity)'; }
{ my %h = a=>1;      my $c = (0, %h); %h.push((b => 2)); is $c[1].keys.sort.join(','), 'a,b', 'hash push visible'; }
{ my %h = a=>1,b=>2,c=>3; my $c = (0, %h); %h<a b>:delete; is $c[1].keys.sort.join(','), 'c', 'hash slice delete visible'; }
{ my %h = a=>{x=>1}; my $c = (0, %h); %h<a><y> = 2;  is $c[1]<a>.keys.sort.join(','), 'x,y', 'nested hash key assign visible'; }

# --- array :delete (with trailing-hole trim) ---
{ my @a = 1,2,3; my $c = (0, @a); @a[2]:delete;      is $c[1].elems, 2, 'array delete + trim visible'; }

# --- identity assertions ---
{
    my @arr = 0..9;
    my $cap = (0, @arr);
    ok $cap[1] =:= @arr, 'capture shares container identity';
    @arr.push(10);
    ok $cap[1] =:= @arr, 'identity preserved across push';
    is $cap[1].elems, 11, 'capture sees the push';
}

# --- closure-captured (shared-cell) variables behave the same ---
{ my @a = 1,2; my $f = { @a.elems };   my $c = (0, @a); @a.push(9); is $c[1].join(','), '1,2,9', 'cell: push visible'; }
{ my @a = 1,2; my $f = { @a.push(9) }; my $c = (0, @a); $f();      is $c[1].join(','), '1,2,9', 'cell: closure push visible'; }
{ my @a = 1,2; my $f = { @a.elems };   my $c = (0, @a); @a[0] = 9; is $c[1].join(','), '9,2',   'cell: element assign visible'; }
{ my @a = 1,2; my $f = { @a[0] = 9 };  my $c = (0, @a); $f();      is $c[1].join(','), '9,2',   'cell: closure element assign visible'; }
{ my %h = a=>1; my $f = { %h.elems };  my $c = (0, %h); %h<b> = 2; is $c[1].keys.sort.join(','), 'a,b', 'cell: hash key assign visible'; }
{ my %h = a=>1; my $f = { %h<b> = 2 }; my $c = (0, %h); $f();      is $c[1].keys.sort.join(','), 'a,b', 'cell: closure hash key assign visible'; }
{ my @a = 1,2; my $f = { @a = 7,8 };   my $c = (0, @a); $f();      is $c[1].join(','), '7,8',   'cell: closure whole reassign visible'; }

# --- typed / defaulted containers (slow-path dispatch) ---
{ my Int @a = 1,2; my $c = (0, @a); @a.push(9); is $c[1].join(','), '1,2,9', 'typed array push visible'; }
{ my Int @a = 1,2; my $c = (0, @a); @a[0] = 9;  is $c[1].join(','), '9,2',   'typed array element assign visible'; }
{ my Int @a = 1,2; my $c = (0, @a); @a.pop;     is $c[1].join(','), '1',     'typed array pop visible'; }
{ my Int %h = a=>1; my $c = (0, %h); %h<b> = 2; is $c[1].keys.sort.join(','), 'a,b', 'typed hash key assign visible'; }
{ my @a is default(42) = 1,2; my $c = (0, @a); @a.push(9); is $c[1].join(','), '1,2,9', 'defaulted array push visible'; }

# --- := bound alias + capture ---
{ my @a = 1,2; my @b := @a; my $c = (0, @a); @b.push(9); is $c[1].join(','), '1,2,9', 'bound-alias push visible through capture'; }

# --- deep structures ---
{ my %h = a=>[1]; my $c = (0, %h); %h<a>.push(2);   is $c[1]<a>.join(','), '1,2', 'array-in-hash push visible'; }
{ my @a = [1,2], [3,4]; my $c = (0, @a); @a[0][0] = 9;      is $c[1][0].join(','), '9,2', 'nested element assign visible'; }
{ my @a = [1,2], [3,4]; my $c = (0, @a); @a[0].splice(0,1); is $c[1][0].join(','), '2',   'element splice mutates in place'; }
{ my @a = [1,2], [3,4]; is @a[0].splice(0,1).join(','), '1', 'element splice returns removed elements'; }

# --- return-writeback shape: sub-local captured into a returned list ---
{
    sub make() { my @inner = 1,2; ( { @inner.push(9) }, (0, @inner) ) }
    my ($cb, $cap) = make();
    $cb();
    is $cap[1].join(','), '1,2,9', 'returned capture tracks later closure mutation';
}

# --- copies stay detached (copy-time detach, not mutation-time COW) ---
{ my @a = 1,2; my @b = @a; @a.push(9);  is @b.join(','), '1,2',   'my @b = @a copy unaffected by source push'; }
{ my @a = 1,2; my @b = @a; @b.push(9);  is @a.join(','), '1,2',   'source unaffected by copy push'; }
{ my %h = a=>1; my %g = %h; %h<b> = 2;  is %g.keys.join(','), 'a', 'hash copy unaffected by source write'; }
{ my @a = 1,2; my @b = @a.clone; @a.push(9); is @b.join(','), '1,2', '.clone unaffected by source push'; }
{
    my @a = 1, 2;
    sub f(@b is copy) { @b.push(9); @b.join(',') }
    is f(@a), '1,2,9', 'is copy param gets its own container';
    is @a.join(','), '1,2', 'is copy param mutation does not reach caller';
}
{
    my %h = a => 1;
    sub g(%b is copy) { %b<z> = 9; %b.keys.sort.join(',') }
    is g(%h), 'a,z', 'is copy hash param gets its own container';
    is %h.keys.join(','), 'a', 'is copy hash param mutation does not reach caller';
}

# --- multidim (semicolon) subscript mutations visible through a capture ---
{ my @a = [1,2],[3,4]; my $c = (0, @a); @a[0;1] = 99;      is $c[1][0][1], 99,  'multidim element assign visible'; }
{ my %h = a=>{x=>1};   my $c = (0, %h); %h{'a';'x'} = 42;  is $c[1]<a><x>, 42,  'multidim hash assign visible'; }
{ my @a = [1,2],[3,4]; my $c = (0, @a); @a[*;*] = 9,8,7,6; is $c[1][1].join(','), '7,6', 'multidim whatever-slice assign visible'; }
{ my @a = [1,2],[3,4]; my $c = (0, @a); @a[0;1]:delete;    is $c[1][0].elems, 1, 'multidim delete visible'; }
{ my @a = [1,2],[3,4]; my $c = (0, @a); @a[0;3] = 9;       is $c[1][0].elems, 4, 'multidim autoviv grow visible'; }

# --- reference holders keep tracking (non-copy '=' targets) ---
{ my @a = 1,2; my @outer; @outer[0] = @a; @a.push(9); is @outer[0].join(','), '1,2,9', 'element holding array tracks push'; }
{ my @a = 1,2; my %h; %h<k> = @a; @a.push(9);         is %h<k>.join(','), '1,2,9',    'hash value holding array tracks push'; }
{ my @a = 1,2; my $s = @a; @a.push(9);                is $s.join(','), '1,2,9',       'scalar holding array tracks push'; }
