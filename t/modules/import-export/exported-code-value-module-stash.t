use Test;

plan 2;

use lib 't/lib';
use ExportedCodeValueStashFixture;

ok &ExportedCodeValueStashFixture::original.defined,
  'the module stash exposes an exported sub';
ok &ExportedCodeValueStashFixture::alias.defined,
  'the module stash exposes an exported code-valued alias';
