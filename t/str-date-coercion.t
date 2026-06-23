use Test;

# Str.Date / Str.DateTime coerce an ISO-formatted string to a Date / DateTime
# (documented Str methods). Str-only — other Cool types do not get them.

plan 18;

# --- Str.Date ---
is "2015-11-24".Date,            "2015-11-24", 'Str.Date stringifies as the date';
isa-ok "2015-11-24".Date,        Date,         'Str.Date returns a Date';
is "2015-11-24".Date.year,       2015,         'Str.Date.year';
is "2015-11-24".Date.month,      11,           'Str.Date.month';
is "2015-11-24".Date.day,        24,           'Str.Date.day';
is "2015-11-24".Date.day-of-week, 2,           'Str.Date.day-of-week';
is "-0044-03-15".Date.year,      -44,          'Str.Date with negative (BCE) year';

# --- Str.DateTime ---
is "2015-12-24T12:23:00Z".DateTime, "2015-12-24T12:23:00Z", 'Str.DateTime full ISO';
isa-ok "2015-12-24T12:23:00Z".DateTime, DateTime,           'Str.DateTime returns a DateTime';
is "2012-02-29T12:34:56Z".DateTime.hour,   12, 'Str.DateTime.hour';
is "2012-02-29T12:34:56Z".DateTime.minute, 34, 'Str.DateTime.minute';
# A bare date (no time component) becomes midnight UTC.
is "2015-11-24".DateTime, "2015-11-24T00:00:00Z", 'Str.DateTime from bare date is midnight';
is "2015-11-24".DateTime.hour, 0, 'bare-date DateTime hour is 0';

# --- round-trip and Cool function-call forms ---
is "2015-11-24".DateTime.Date, "2015-11-24", 'DateTime.Date round-trips';
is Date("2015-11-24").year,    2015,         'Date(Str) function form';
is DateTime("2012-02-29T12:34:56Z").hour, 12, 'DateTime(Str) function form';

# --- error handling ---
throws-like { "abc".Date }, X::Temporal::InvalidFormat,
    'malformed Str.Date throws InvalidFormat';
throws-like { "garbage".DateTime }, X::Temporal::InvalidFormat,
    'malformed Str.DateTime throws InvalidFormat';
