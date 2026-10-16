use Test;

# `Date.today`, `Date.new-from-daycount` and `DateTime.now` are rows of the
# method table, found by their owner for the type object of the class or a
# subclass (ADR-11276 §9.123). A type object never reaches an instance row.

plan 10;

class D is Date { }
class DT is DateTime { }

isa-ok Date.today, Date, 'Date.today';
is D.today.^name, 'D', 'a subclass builds its own type';
is Date.new-from-daycount(59000), '2020-05-31', 'Date.new-from-daycount';
is D.new-from-daycount(59000).^name, 'D', 'new-from-daycount of a subclass';
isa-ok DateTime.now, DateTime, 'DateTime.now';
is DT.now.^name, 'DT', 'now of a subclass';
is DateTime.now(:timezone(3600)).timezone, 3600, 'now takes :timezone';
is Date.today, DateTime.now.Date, 'today is the date of now';

dies-ok { D.year }, 'an instance row refuses a subclass type object';
dies-ok { Date.year }, 'an instance row refuses the type object';
