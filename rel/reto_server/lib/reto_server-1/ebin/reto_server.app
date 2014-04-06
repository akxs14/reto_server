%% app generated at {2014,4,6} {19,39,48}
{application,reto_server,
             [{description,"reto server"},
              {vsn,"1"},
              {id,[]},
              {modules,[ad_request_handler,openrtb2_bid_request_handler,
                        openrtb2_bid_request_parser,reto_launch,reto_server,
                        reto_server_app,reto_server_sup,router,
                        router_handler]},
              {registered,[]},
              {applications,[kernel,stdlib,cowboy]},
              {included_applications,[]},
              {env,[{http_port,8080}]},
              {maxT,infinity},
              {maxP,infinity},
              {mod,{reto_server_app,[]}}]}.

