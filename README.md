# CouchDB replication stats plugin

This plugin fetches and compiles basic statistics about replications declared in
the `_replicator` database.
The execution time and performance toll of this plugin is way smaller than fetching
the data (from `_active_tasks` and views on `_replicator` database) and compiling the stats on the
'client-side', making it suitable for running as a part of monitoring solution.

Statistics are exposed as `/_replication_stats` HTTP endpoint. Issuing GET request as an admin
returns the following response:

```json
{
  "active_replications": 120,
  "running_replications": 120,
  "oldest_update": 1453104981,
  "time_since_oldest_update": 1,
  "total_revisions_checked": 0,
  "total_missing_revisions_found": 0,
  "total_write_failures": 0,
  "total_writes": 0,
  "writes_in_progress": 0
}
```

Where:

* `active_replications` is the total number of non-cancelled replications in `_replicator` database.
Please note that this does not include replications that were automatically cancelled after reaching
maximum number of retries (in row) due to failure (which is 10 by default).
* `running_replications` is the total number of replications for which there is a replicator process
running correctly. If this number is smaller than `active_replications`, then some replications have
died or are failing.
* `oldest_update` is the oldest among the reported timestamps (in CouchDB time) of replication
updates. Update timestamp is recorded every time a given replication process checks the source
database for updates and enqueues any missing revisions for copying.
* `time_since_oldest_update` is the number of seconds since `oldest_update` (in CouchDB time).
If it is constantly greater than the maximum `checkpoint_interval` for the replications,
it means that the replication processes may be starved by other database operations.
* `total_revisions_checked` is the total number of revisions read from the source databases
(since the start of the database instance).
* `total_missing_revisions_found` is the total number of revisions that were to be copied to the
target databases (since the start of the database instance).
* `total_missing_revisions_found` is the total number of revisions that copied to the
target databases (since the start of the database instance).
* `writes_in_progress` is the the number of replications that are in the process of copying the
revisions (i.e. for which `missing_revisions_found` > `docs_written`).

## Installation

TODO

## License

Copyright (c) 2016 Evidence Prime, Inc.
See the LICENSE file for license rights and limitations (MIT).
