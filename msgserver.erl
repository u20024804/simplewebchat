-module(msgserver).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([dispatch/3, popmsg/1]).

-define(TTL, 15000).
-define(DELAY, 10).
-define(GEN_CAST, '$gen_cast').

-import(lists, [foreach/2]).

-include("head.hrl").
-import(whereserver, [where/1, whois/1]).
-import(channelserver, [userlist/1]).
-import(util, [do/1]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_) ->
    {ok, message}.
    
save_msg(Message) ->
    mnesia:transaction(fun() -> mnesia:write(Message) end).

offline_msg(Who) ->
    {atomic, Messages} = mnesia:transaction(fun() -> 
            mnesia:read({message, Who}) end),
    Messages.
    
remove_msg(Messages) ->
    F = fun() ->
            foreach(fun(Message) ->
                    mnesia:delete_object(Message)
                end, Messages)
        end,
    mnesia:transaction(F).

push(Sender, Receiver, Msg) ->
    Message = #message{receiver=Receiver, sender=Sender,
            content=Msg, time=time(), tag=make_ref()},
    case where(Receiver) of
        {address, notfound} ->
            save_msg(Message);
        {address, Receiver, From} ->
            Tag = make_ref(),
            From ! {self(), Tag, [Message]},
            receive
                {Tag, finish} ->
                    finish
            after ?DELAY ->
                save_msg(Message)
            end
    end.
    
handle_cast({dispatch, Owner, {channel, friend, Friend}, Msg}, Msgs) ->
    Sender = {sender, friend, Owner},
    push(Sender, Friend, Msg),
    {noreply, Msgs};
    
handle_cast({dispatch, Owner, {channel, group, Group}, Msg}, Msgs) ->
    Sender = {sender, group, Group, Owner},
    foreach(fun(Member) ->
            push(Sender, Member, Msg)
        end, userlist(Group)),
    {noreply, Msgs};
    
handle_cast({pop, Who, From}, Msgs) ->
    case offline_msg(Who) of
        [] ->
            void;
        Messages ->
            Tag = make_ref(),
            From ! {self(), Tag, Messages},
            receive
                {Tag, finish} ->
                    remove_msg(Messages)
            after ?DELAY ->
                void
            end
    end,
    {noreply, Msgs}.
        
handle_call(_Request, _From, State) ->
    {reply, void, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
dispatch(Owner, Channel, Msg) ->
    gen_server:cast(?MODULE, {dispatch, Owner, Channel, Msg}).
    
popmsg(Who) ->
    gen_server:cast(?MODULE, {pop, Who, self()}),
    receive
        {Server, Tag, Messages} ->
            Server ! {Tag, finish},
            Messages
    after ?TTL ->
        []
    end.

