use Test;

# Regression from Timezones::ZoneInfo::State (Timezone::Simple).
plan 1;

lives-ok { die 'unreachable', 42 unless True },
    'a die statement with multiple arguments honors its statement modifier';
