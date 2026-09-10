use v6;
use Test;

plan 9;

# Object-type reprs from PLAN.md §8.15 (repr oracle sweep): Channel rendered
# as the bare type name, and the schedulers/Supplier hid their raku-visible
# attributes.

is Channel.new.raku, 'Channel.new', 'Channel.raku is the constructor form';
is Channel.new.gist, 'Channel.new', 'Channel.gist matches';
is [Channel.new].raku, '[Channel.new]', 'Channel nested in an array .raku';
is [Channel.new].gist, '[Channel.new]', 'Channel nested in an array .gist';

is ThreadPoolScheduler.new.raku,
    'ThreadPoolScheduler.new(uncaught_handler => Callable)',
    'ThreadPoolScheduler shows its unset uncaught_handler';
is CurrentThreadScheduler.new.raku,
    'CurrentThreadScheduler.new(uncaught_handler => Callable)',
    'CurrentThreadScheduler likewise';
is ThreadPoolScheduler.new.gist,
    'ThreadPoolScheduler.new(uncaught_handler => Callable)',
    'scheduler gist matches the raku form';

is Supplier.new.raku, 'Supplier.new(taplist => Supplier::TapList.new)',
    'Supplier shows its taplist';
is [Supplier.new].raku, '[Supplier.new(taplist => Supplier::TapList.new)]',
    'Supplier nested in an array';
