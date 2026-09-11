use v6;
use Test;

# `::?CLASS` and `::?ROLE` are resolved when a role method is copied into its
# consuming class. They must work for both named and anonymous parameters.

plan 8;

role PseudoTypes {
    method named-role(::?ROLE $value) { 'role' }
    method named-class(::?CLASS $value) { 'class' }
    method anonymous-role(::?ROLE) { 'anonymous role' }
    method anonymous-class(::?CLASS) { 'anonymous class' }
}

class Consumer does PseudoTypes { }
class Foreign { }

my $consumer = Consumer.new;
my $foreign = Foreign.new;

is $consumer.named-role($consumer), 'role',
    'a named ::?ROLE parameter accepts the consuming class';
is $consumer.named-class($consumer), 'class',
    'a named ::?CLASS parameter accepts the consuming class';
is $consumer.anonymous-role($consumer), 'anonymous role',
    'an anonymous ::?ROLE parameter accepts the consuming class';
is $consumer.anonymous-class($consumer), 'anonymous class',
    'an anonymous ::?CLASS parameter accepts the consuming class';

dies-ok { $consumer.named-role($foreign) },
    'a named ::?ROLE parameter rejects a foreign class';
dies-ok { $consumer.named-class($foreign) },
    'a named ::?CLASS parameter rejects a foreign class';
dies-ok { $consumer.anonymous-role($foreign) },
    'an anonymous ::?ROLE parameter rejects a foreign class';
dies-ok { $consumer.anonymous-class($foreign) },
    'an anonymous ::?CLASS parameter rejects a foreign class';
