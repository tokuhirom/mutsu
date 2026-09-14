our $tm_sec is export(:FIELDS);

class Time::localtime {
    has Int $.sec;
}

sub localtime() is export(:DEFAULT:FIELDS) {
    $tm_sec = 7;
    Time::localtime.new(sec => $tm_sec)
}
