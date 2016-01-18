-module(replication_stats_plugin_httpd).

-export([handle_req/1]).

-include_lib("couch/include/couch_db.hrl").

handle_req(#httpd{method = 'GET'} = Req) ->
  ok = couch_httpd:verify_is_server_admin(Req),
  couch_httpd:send_json(Req, {replication_stats_plugin:stats()});
handle_req(Req) ->
  couch_httpd:send_method_not_allowed(Req, "GET").
