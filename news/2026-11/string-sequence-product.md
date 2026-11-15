String sequences with equal-width endpoints now traverse the product of each
character position's range, including positions that descend while others
ascend. A longer seed stops immediately when increasing toward a shorter
endpoint, while a descending sequence can supply a finite head before an
eventual predecessor underflow.
