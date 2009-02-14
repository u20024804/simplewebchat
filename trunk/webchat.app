{application, webchat, 
    [{descritpion, "Simple Web Chat"},
    {vsn, "0.0.1.1"},
    {modules, [
        channelserver,
        msgserver,
        whereserver,
        httpserver,
        phrase,
        webchat_sup,
        webchat_app
        ]},
    {mod, {webchat_app, []}},
    {registered, []},
    {applications, [kernel, stdlib, mnesia]},
    {env, []}
    ]}.
