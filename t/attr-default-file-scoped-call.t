use lib 't/lib';
use Test;
use Issue7733::Conf;
use Issue7733::User;

plan 4;

my $direct = Issue7733::Conf.new;
is $direct.attribute-default, 'direct',
   'an attribute default sees a file-scoped sub from the mainline';
is $direct.build-default, 'direct',
   'a BUILD parameter default sees a file-scoped sub from the mainline';

my $foreign = Issue7733::User.new;
is $foreign.config.attribute-default, 'direct',
   'an attribute default sees its file-scoped sub across a module boundary';
is $foreign.config.build-default, 'direct',
   'a BUILD parameter default sees its file-scoped sub across a module boundary';
