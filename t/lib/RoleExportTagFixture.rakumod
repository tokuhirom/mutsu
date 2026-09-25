unit module RoleExportTagFixture;

# Fixture for t/modules/import-export/role-is-export-tag.t: roles exported
# under named tags must declare those tags, exactly as tagged classes do.
role TagRole is export(:T) { method who { 'TagRole' } }
our role OurTagRole is export(:T) { method who { 'OurTagRole' } }
role Base { method base { 'base' } }
my role Tagged does Base is export(:Tagged) { method who { 'Tagged' } }
role TwoTags is export(:A, :B) { method who { 'TwoTags' } }
role DefaultRole is export { method who { 'DefaultRole' } }
