use v6.d;
use Test;
use lib 't/lib';

plan 7;

use ProtoMultiMismatchedUnitName;
use ProtoMultiMismatchedConsumer;
use ProtoMultiMismatchedConsumerTwo;
use Math::Fitting::LinearRegression;

is mismatched-export(42), 'int:42',
    'an exported proto imports a later Int candidate';
is mismatched-export('answer'), 'str:answer',
    'an exported proto imports a later Str candidate';
is mismatched-export-capture([1, 2], 2), 'uint:2',
    'an exported capture proto imports its UInt candidate';
is call-mismatched-export([1, 2], 2), 'uint:2',
    'a transitive module import retains the exported proto candidates';
is call-mismatched-export-two([1, 2], 2), 'uint:2',
    'a second importing module also receives the exported family';
is probe-is-positional([1, 2], 2), True,
    'the Math::Fitting namespace topology retains the exported family';
is probe-fit([(1, 2)], :prop('Function')), 'fit',
    'an exported predicate is callable from a multi where constraint';
