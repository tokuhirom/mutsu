use Test;

# Core roles modelled natively (Blob, Buf, Positional) report the metaobjects
# rakudo gives them: a group HOW, one ParametricRoleHOW candidate, and a
# CurriedRoleHOW once parameterized. Found with the `Kind` distribution
# (t/01-typecheck.t, "roles" subtest).
plan 14;

my constant PFX = 'Perl6::Metamodel::';

is Blob.HOW.^name, PFX ~ 'ParametricRoleGroupHOW', 'core role group HOW';
is Blob.^candidates.elems, 1, 'a core role has one candidate';
is Blob.^candidates[0].HOW.^name, PFX ~ 'ParametricRoleHOW', 'its candidate HOW';
is Positional.^candidates[0].HOW.^name, PFX ~ 'ParametricRoleHOW', 'Positional candidate HOW';
is Blob[uint8].HOW.^name, PFX ~ 'CurriedRoleHOW', 'curried Blob HOW';
is Buf[uint8].HOW.^name, PFX ~ 'CurriedRoleHOW', 'curried Buf HOW';
is Blob[uint8].new.^roles[0].HOW.^name, PFX ~ 'CurriedRoleHOW', 'instance role is curried';

role Pr[::T] { }
is Pr.^candidates[0].HOW.^name, PFX ~ 'ParametricRoleHOW', 'declared role candidate unchanged';
is Pr[Int].HOW.^name, PFX ~ 'CurriedRoleHOW', 'declared curried role unchanged';

is Blob[uint8].new.^roles.map(*.^name).join(' '),
   'Blob[uint8] UnsignedBlob[uint8] Positional[uint8] Stringy UnsignedBlob[uint8]',
   'Blob[uint8] instance .^roles';
is Buf[int32].new.^roles.map(*.^name).join(' '),
   'Buf[int32] Buf::SignedBuf[int32] Blob[int32] SignedBlob[int32] Positional[int32] Stringy SignedBlob[int32] Buf::SignedBuf[int32]',
   'Buf[int32] instance .^roles';
is Blob[uint8].^roles.map(*.^name).join(' '),
   'UnsignedBlob[uint8] Positional[uint8] Stringy',
   'Blob[uint8] type object .^roles';
is Buf[uint8].^roles(:!transitive).map(*.^name).join(' '),
   'Buf::UnsignedBuf[uint8] Blob[uint8]',
   'Buf[uint8] type object .^roles(:!transitive)';
is Blob[int8].new.^roles(:!transitive).map(*.^name).join(' '),
   'Blob[int8] SignedBlob[int8]',
   'Blob[int8] instance .^roles(:!transitive)';
