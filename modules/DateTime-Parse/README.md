DateTime::Parse
===================

DateTime parser.

[![test](https://github.com/sergot/datetime-parse/actions/workflows/test.yml/badge.svg)](https://github.com/sergot/datetime-parse/actions/workflows/test.yml)


Synopsis
===================


        my $rfc1123 = DateTime::Parse.new('Sun, 06 Nov 1994 08:49:37 GMT');
        say $rfc1123.Date;

        say DateTime::Parse.new('Sun', :rule<wkday>) + 1; # 7th day of week



Description
==================

- new( $format, $rule )
    - $format : String with DateTime to be parsed
    - $rule   : Rule to run
