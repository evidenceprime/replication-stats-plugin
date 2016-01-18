-module(replication_stats_plugin).

-export([stats/0]).

-import(couch_util, [get_value/2]).

-define(REP_TO_STATE, couch_rep_id_to_rep_state).
-define(record_to_tuplelist(Rec, Ref), lists:zip(record_info(fields, Rec), tl(tuple_to_list(Ref)))).

-record(stats, {
  running_replications = 0,
  oldest_update = timestamp(),
  total_revisions_checked = 0,
  total_missing_revisions_found = 0,
  total_write_failures = 0,
  total_writes = 0,
  writes_in_progress = 0
 }).

is_write_in_progress(TaskProps) ->
  RevsToWrite = get_value(missing_revisions_found, TaskProps),
  RevsWritten = get_value(docs_written, TaskProps),
  if
    RevsToWrite > RevsWritten -> 1;
    true -> 0
  end.

update_stats(#stats{
    running_replications = Count,
    oldest_update = OldestUpdate,
    total_revisions_checked = TotalRevisionsChecked,
    total_missing_revisions_found = TotalMissingRevisionsFound,
    total_write_failures = TotalWriteFailures,
    total_writes = TotalWrites,
    writes_in_progress = WritesInProgress}, TaskProps) ->
  #stats{
    running_replications = Count + 1,
    oldest_update = min(OldestUpdate, get_value(updated_on, TaskProps)),
    total_revisions_checked = TotalRevisionsChecked + get_value(revisions_checked, TaskProps),
    total_missing_revisions_found = TotalMissingRevisionsFound +
      get_value(missing_revisions_found, TaskProps),
    total_write_failures = TotalWriteFailures + get_value(doc_write_failures, TaskProps),
    total_writes = TotalWrites + get_value(docs_written, TaskProps),
    writes_in_progress = WritesInProgress + is_write_in_progress(TaskProps)
  }.

stats() ->
  DeclaredReplicationsCount = ets:info(?REP_TO_STATE, size), 
  Stats = lists:foldl(fun(TaskProps, Acc) ->
    case get_value(type, TaskProps) of
      replication -> update_stats(Acc, TaskProps);
      _ -> Acc
    end
  end, #stats{}, couch_task_status:all()),
  ?record_to_tuplelist(stats, Stats) ++ [
    {time_since_oldest_update, timestamp() - Stats#stats.oldest_update},
    {active_replications, DeclaredReplicationsCount}].

timestamp() ->
    timestamp(now()).

timestamp({Mega, Secs, _}) ->
    Mega * 1000000 + Secs.
