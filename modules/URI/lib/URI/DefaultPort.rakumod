# This logic seems to belong somewhere related to URI but not in the URI
# module itself.

unit class URI::DefaultPort;

my Int %default_port = (
    ftp     =>      21,
    sftp    =>      22,
    ssh     =>      22,
    telnet  =>      23,
    tn3270  =>      23,
    smtp    =>      25,
    gopher  =>      70,
    http    =>      80,
    shttp   =>      80,
    pop     =>      110,
    news    =>      119,
    nntp    =>      119,
    imap    =>      143,
    ldap    =>      389,
    https   =>      443,
    rlogin  =>      513,
    rtsp    =>      554,
    rtspu   =>      554,
    snews   =>      563,
    ldaps   =>      636,
    rsync   =>      873,
    mms     =>      1755,
    sip     =>      5060,
    sips    =>      5061,
    git     =>      9418
);
    
method scheme-port(Str $scheme) {
    %default_port{$scheme};
}

# vim: expandtab shiftwidth=4
