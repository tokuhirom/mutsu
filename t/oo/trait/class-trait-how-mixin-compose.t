use Test;

plan 12;

# A class trait that mixes a role into the class's own HOW
# (`$class.HOW does SomeRole`) gets that role's `compose` override run when
# the class composes, and the override's `callsame` reaches the native
# ClassHOW `compose`. This is the Staticish distribution's singleton
# mechanism: its `MetamodelX::StaticHOW.compose` wraps every method of the
# class so a call on the type object is redirected to the one instance.

role LoggingHOW {
    my %skip = :new, :bless;
    my @log;
    method logged { @log.join(' ') }
    method redirect($self: |c) {
        my $inst = $self;
        $inst = $self.new unless $inst.defined;
        callwith($inst, |c);
    }
    method compose(Mu $obj) {
        @log.push: 'before:' ~ ($obj.^method_table<attr>:exists);
        callsame;
        @log.push: 'after:' ~ ($obj.^method_table<attr>:exists);
        for $obj.^method_table.kv -> $name, $code {
            next if %skip{$name}:exists;
            $code.wrap(self.^find_method('redirect'));
        }
    }
}

role OneInstance {
    my $instance;
    method new(|c) { $instance //= self.bless(|c) }
}

multi sub trait_mod:<is>(Mu:U $type, :$singleton!) {
    $type.HOW does LoggingHOW;
    $type.^add_role(OneInstance);
}

class Counter is singleton {
    has $.attr;
    method greet(Str $who = "you") { "hello $who from $!attr" }
}

is Counter.HOW.logged, 'before:False after:True',
    'the mixed-in compose runs once; accessors appear only after its callsame';
ok Counter.HOW ~~ LoggingHOW, 'the class HOW carries the mixed-in role';

my $c = Counter.new(attr => 'the one');
ok $c === Counter.new, 'the singleton constructor is untouched by the wrapping';
is $c.greet, 'hello you from the one', 'a wrapped method still runs on an instance';
is Counter.greet, 'hello you from the one',
    'a wrapped method called on the type object reaches the instance';
is Counter.greet('me'), 'hello me from the one', 'arguments pass through the wrapper';
is Counter.attr, 'the one', 'a wrapped auto-accessor called on the type object reaches the instance';

# The role-body `my %skip` was first initialized while the trait ran inside
# the class declaration; it must stay visible to the role's methods after.
my $how = Counter.HOW;
my $m = $how.^find_method('redirect');
isa-ok $m, Method, '.^find_method on a HOW mixin finds the role method';
is $m.name, 'redirect', 'and it is the right one';

# A role's own body lexicals stay reachable from a mixed-in method when the
# first `does` happened in a frame that has since returned.
role Keeps { my %seen = :a, :b; method keys-seen { %seen.keys.sort.join(',') } }
sub apply-keeps($obj) { $obj does Keeps }
my $plain = class { }.new;
apply-keeps($plain);
is $plain.keys-seen, 'a,b', 'a role-body lexical survives the applying frame';

# `.^find_method` / `.^lookup` on an ordinary `but` mixin.
role Extra { method extra { 'extra' } }
my $mixed = Any.new but Extra;
isa-ok $mixed.^find_method('extra'), Method, '.^find_method finds a mixed-in role method';
is $mixed.^lookup('extra')($mixed), 'extra', '.^lookup result is callable with the invocant';
