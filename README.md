

# taskforce #

Copyright (c) 2015 Guilherme Andrade

__Version:__ 1.0.0

__Authors:__ Guilherme Andrade ([`g@gandrade.net`](mailto:g@gandrade.net)).

`taskforce` allows you to parallelise arbitrary tasks in a controlled way.

---------


```erlang

application:ensure_all_started(taskforce),
application:ensure_all_started(inets),
application:ensure_all_started(ssl),

DownloadFun = fun(Id, Url) ->
    io:format("~p Trying to download '~p' @ ~p..~n", [os:timestamp(), Id, Url]),
    {ok, Result} = httpc:request(Url),
    io:format("~p Handled '~p'~n", [os:timestamp(), Id]),
    Result
end,

IdentifiedUrls = [{google_homepage, "https://www.google.com/"},
                  {ip_over_avian_carriers_rfc, "https://www.ietf.org/rfc/rfc1149.txt"},
                  {hackernews_rss, "https://news.ycombinator.com/rss"},
                  {example_image1, "https://assets.crowdsurge.com/datacapture/example/img/example_logo.png"},
                  {example_image2, "http://underthebridge.co.uk/wp-content/uploads/2014/03/Example-main-image1.jpg"},
                  {example_image3, "https://assets-cdn.github.com/images/modules/logos_page/GitHub-Mark.png"}],

IndividualTimeoutT = 2000, % in miliseconds
GlobalTimeoutT = 10000,    % in miliseconds
MinionCount = 4,          % 4 workers

Tasks = [taskforce:new_task({download_result, Id}, DownloadFun, [Id, Url], 
                            IndividualTimeoutT)
         || {Id, Url} <- IdentifiedUrls],

{ResultsById, Individualtimeouts, GlobalTimeouts} = taskforce:execute_tasks(
      Tasks, GlobalTimeoutT, MinionCount).

% {1430,311507,997903} Trying to download 'example_image2' @ "http://underthebridge.co.uk/wp-content/uploads/2014/03/Example-main-image1.jpg"..
% {1430,311507,997915} Trying to download 'hackernews_rss' @ "https://news.ycombinator.com/rss"..
% {1430,311507,997925} Trying to download 'example_image3' @ "https://assets-cdn.github.com/images/modules/logos_page/GitHub-Mark.png"..
% {1430,311507,997936} Trying to download 'example_image1' @ "https://assets.crowdsurge.com/datacapture/example/img/example_logo.png"..
% {1430,311508,446044} Handled 'example_image2'
% {1430,311508,448227} Trying to download 'ip_over_avian_carriers_rfc' @ "https://www.ietf.org/rfc/rfc1149.txt"..
% {1430,311508,462708} Handled 'hackernews_rss'
% {1430,311508,463315} Trying to download 'google_homepage' @ "https://www.google.com/"..
% {1430,311508,771638} Handled 'example_image1'
% {1430,311508,810445} Handled 'google_homepage'
% {1430,311509,58815} Handled 'example_image3'
% {1430,311509,447087} Handled 'ip_over_avian_carriers_rfc'


lists:keyfind({download_result, hackernews_rss}, 1, ResultsById).

% {{download_result,hackernews_rss},
%  {{"HTTP/1.1",200,"OK"},
%   [{"cache-control","private"},
%    {"connection","keep-alive"},
%    {"date","Wed, 29 Apr 2015 12:45:08 GMT"},
%    {"server","cloudflare-nginx"},
%    {"content-length","10170"},
%    {"content-type","application/rss+xml; charset=utf-8"},
%    {"set-cookie",
%     "__cfduid=dfee6cd1ba6238c5dbf33583317f1448b1430311508; expires=Thu, 28-Apr-16 12:45:08 GMT; path=/; domain=.ycombinator.com; HttpOnly"},
%    {"x-frame-options","DENY"},
%    {"strict-transport-security",
%     "max-age=31556900; includeSubDomains"},
%    {"cf-ray","1deb1a2eba8b0ded-MAD"}],
%   [60,114,115,115,32,118,101,114,115,105,111,110,61,34,50,46,
%    48,34,62,60,99,104,97,110|...]}}

```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/develop/doc/taskforce.md" class="module">taskforce</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/develop/doc/taskforce_app.md" class="module">taskforce_app</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/develop/doc/taskforce_sup.md" class="module">taskforce_sup</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/develop/doc/tf_master_serv.md" class="module">tf_master_serv</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/develop/doc/tf_master_sup.md" class="module">tf_master_sup</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/develop/doc/tf_minion_serv.md" class="module">tf_minion_serv</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/develop/doc/tf_minion_sup.md" class="module">tf_minion_sup</a></td></tr></table>

