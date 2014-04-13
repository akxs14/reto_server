%% app generated at {2014,4,13} {3,22,45}
{application,reto_server,
             [{description,"reto server"},
              {vsn,"1"},
              {id,[]},
              {modules,[ad_request_handler,data_augmentor,decision_engine,
                        openrtb2_bid_request_handler,
                        openrtb2_bid_request_parser,reto_server,
                        reto_server_app,reto_server_sup,router]},
              {registered,[]},
              {applications,[kernel,stdlib,cowboy,jiffy]},
              {included_applications,[]},
              {env,[{http_port,8080}]},
              {maxT,infinity},
              {maxP,infinity},
              {mod,{reto_server_app,[]}}]}.

