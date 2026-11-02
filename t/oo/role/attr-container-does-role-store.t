use Test;

plan 7;

# `%!h does R` / `@!a does R` mixes the role into the attribute's own
# container, and an assignment to it then goes through the role's `STORE`
# (#16014).
my @log;
role Logs { method STORE(\v) { @log.push: 'STORE'; self } }

class K {
    has %.h;
    has @.a;
    submethod TWEAK { %!h does Logs; @!a does Logs }
    method chk-h { %!h.does(Logs) }
    method chk-a { @!a.does(Logs) }
    method put-h { %!h = (a => 1) }
    method put-a { @!a = 3, 4 }
}

my $k = K.new;
ok $k.chk-h, 'the hash attribute container carries the role';
ok $k.chk-a, 'the array attribute container carries the role';

$k.put-h;
is @log.elems, 1, 'assigning to the hash attribute calls STORE';
$k.put-a;
is @log.elems, 2, 'assigning to the array attribute calls STORE';
ok $k.chk-h && $k.chk-a, 'the roles survive the assignments';

class Plain {
    has %.h;
    method put { %!h = (a => 1); %!h }
}
is-deeply Plain.new.put, %(a => 1), 'an attribute without a role is unchanged';

my %lex does Logs;
%lex = (b => 2);
is @log.elems, 3, 'the lexical form still calls STORE';
