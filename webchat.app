{application, webchat, 
    [{descritpion, "Simple Web Chat"},
    {vsn, "0.0.1.1"},
    {modules, [
        channelserver,
        msgserver,
        whereserver,
        httpserver,
        sessionserver,
        phrase,
        config,
        setup,
        util,
        processer,
        webchat_sup,
        webchat_app,
        webchat
        ]},
    {mod, {webchat_app, []}},
    {registered, []},
    {applications, [kernel, stdlib, mnesia]},
    {env, []}
    ]}.
