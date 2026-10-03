use Test;

# A bare declaration (no initializer) followed by a loose word-logical:
# `my %h andthen do { ... }` is `(my %h) andthen ...`, because the
# word-logicals are looser than the declarator. mutsu used to stop after the
# declaration and die with "Confused. Two terms in a row".
# Found in Net::HTTP's Net/HTTP/Transport.rakumod (raku-mailgun's dependency):
#   my %header andthen do { %header{...}.append(...) for @header-lines>>.split(':', 2) }

plan 9;

my @lines = "A: 1", "b: 2", "a: 3";
my %header andthen do { %header{.[0].lc}.append(.[1].trim-leading) for @lines>>.split(':', 2) }
is-deeply %header<a>, [" 1".trim-leading, "3"], 'my %h andthen do { ... } fills the fresh hash';
is %header.elems, 2, 'the hash declared by the andthen statement stays in scope';

my @log;
my $x andthen @log.push('andthen-ran');
is-deeply @log, [], 'my $x andthen ... skips the tail for an undefined scalar';
ok !$x.defined, '$x is declared';

my $y orelse @log.push("orelse:{$_.raku}");
is-deeply @log, ['orelse:Any'], 'my $y orelse ... runs the tail with the fresh variable as topic';

my @a or @log.push('or-ran');
is @log.elems, 2, 'my @a or ... runs the tail for an empty array';

my %h and @log.push('and-ran');
is @log.elems, 2, 'my %h and ... skips the tail for an empty hash';

my Int $t notandthen @log.push("notandthen:{$_.raku}");
is @log[*-1], 'notandthen:Int', 'my Int $t notandthen ... sees the type object as topic';

my $z = 0;
my $w xor $z = 1;
is $z, 1, 'my $w xor ... evaluates the tail';
