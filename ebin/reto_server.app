{application,reto_server,
             [{description,"reto server"},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{reto_server_app,[]}},
              {env,[{http_port,8080}]},
              {modules,[reto_launch,reto_server,reto_server_app,
                        reto_server_sup]}]}.
