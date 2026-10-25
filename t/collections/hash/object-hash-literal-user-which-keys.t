use Test;

# An object hash files its key objects under `.WHICH`, a user-declared one
# included, whichever way the entry arrives: element assignment, a `:{ }`
# literal or a typed initializer. The literal used to key a user-`WHICH`
# object by identity, so a later `%h{$equal-object} = ...` made a second entry
# (Red's `what-does-it-do` builds `:{ $ast.clone => $value }` and then stores
# under the same AST, see Red 0.2.7 t/05-better-map, t/51-pre-create-or-update).

plan 8;

class K {
    has $.n;
    multi method WHICH(K:D:) { ValueObjAt.new("K|{$!n}") }
}

my $a = K.new(n => 1);
my $b = K.new(n => 1);
my $c = K.new(n => 2);

my %by-assign{Any};
%by-assign{$a} = 1;
%by-assign{$b} = 2;
is %by-assign.elems, 1, 'element assignment: equal WHICH is one entry';

my %literal := :{ $a => 1 };
%literal{$b} = 2;
is %literal.elems, 1, 'literal: a later equal-WHICH store hits the same entry';
is %literal{$a}, 2, 'and replaces its value';

my %cloned := :{ $a.clone => 1 };
%cloned{$a} = 2;
is %cloned.elems, 1, 'a cloned key object is the same key';

my %typed{Any} = $a => 1;
%typed{$b} = 2;
is %typed.elems, 1, 'typed initializer: same';

my %distinct := :{ $a => 1, $c => 2 };
is %distinct.elems, 2, 'different WHICH stay distinct';

my %first := :{ $a => False };
%first{ %first.keys.head } = True;
is %first.elems, 1, 'storing under a key read back from the hash';
is %first{$b}, True, 'is visible through an equal object';
