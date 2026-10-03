use v6;
use lib 't/lib';
use Test;

# A module routine's bare `Error` is the class its own package declares,
# even when its caller imported an enum with an `Error` member (MCP: the
# test imports MCP::Types' `LogLevel::Error`, and MCP::JSONRPC's
# `Error.from-hash` must still reach MCP::JSONRPC::Error).

use CallerEnumWithError;
use OwnTypeOverCallerEnum;

plan 3;

is own-error-kind(), 'own class', "the module's own type wins inside its routine";
is Error, 'error', "the caller's bare Error is still its enum member";
is Debug.value, 'debug', 'other enum members are unaffected';
