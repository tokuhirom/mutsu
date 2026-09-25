use v6;
use lib 't/lib';
use Test;

# Regression (#9372, PDF::COS::Tie's `my role COSDictAttrHOW ... is
# export(:COSDictAttrHOW)`): a role's `is export(:TAG ...)` declares each named
# tag, exactly as a class's does. Previously the role parser recorded only
# DEFAULT, so `use M :TAG` died with "no such tag 'TAG' declared".

plan 7;

{
    use RoleExportTagFixture :T;
    is TagRole.^name, 'RoleExportTagFixture::TagRole', 'role is export(:T) is imported under :T';
    is OurTagRole.^name, 'RoleExportTagFixture::OurTagRole', 'our role is export(:T) is imported under :T';
}

{
    use RoleExportTagFixture :Tagged;
    is Tagged.^name, 'RoleExportTagFixture::Tagged', 'my role ... does Base is export(:Tagged)';
    is Tagged.new.base, 'base', 'the tagged role still composes its parent role';
}

{
    use RoleExportTagFixture :B;
    is TwoTags.new.who, 'TwoTags', 'a role with two tags is importable under the second one';
}

{
    use RoleExportTagFixture :ALL;
    is TagRole.new.who ~ ',' ~ DefaultRole.new.who, 'TagRole,DefaultRole',
        ':ALL imports tagged and DEFAULT roles';
}

{
    use UnitRoleExportTagFixture :U;
    is UnitRoleExportTagFixture.new.who, 'UnitRole', 'unit role is export(:U) declares :U';
}
