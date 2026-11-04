unit module Universal::CompoundOwnShortEnumCodes;

enum Errno is export ($*KERNEL.name eq 'linux' ?? do { EPERM => 1, ENOENT => 2 } !! (A => 1));
