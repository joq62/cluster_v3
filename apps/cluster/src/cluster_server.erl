%%% -------------------------------------------------------------------
%%% Author  : joqerlang
%%% Description :
%%% load,start,stop unload applications in the pods vm
%%% supports with services
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(cluster_server).  

-behaviour(gen_server).  

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/logger.hrl").

%% --------------------------------------------------------------------
-define(SERVER,?MODULE).
%-define(DeplSpecExtension,".depl_spec").

%% External exports
-export([
	 delete_cluster/0,
	 read_state/0,

	 appl_start/1,
	 ping/0
	]).


-export([
	 start/0,
	 stop/0
	]).


-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
		cluster_id=undefined,
		cookie=undefined,
		start_time=undefined
	       }).

%% ====================================================================
%% External functions
%% ====================================================================
appl_start([])->
    application:start(cluster).

%% ====================================================================
%% Server functions
%% ====================================================================
%% Gen server functions

start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).

%% ====================================================================
%% Application handling
%% ====================================================================

    
%%---------------------------------------------------------------
%% Function:delete_pod
%% @doc:delete pod PodNode and PodDir          
%% @param: PodNode
%% @returns:ok|{error,Reason}
%%
%%---------------------------------------------------------------
-spec delete_cluster()-> ok|{error,term()}.
delete_cluster()->
    gen_server:call(?SERVER, {delete_cluster},infinity).



%%---------------------------------------------------------------
%% Function:template()
%% @doc: service spec template  list of {app,vsn} to run      
%% @param: 
%% @returns:[{app,vsn}]
%%
%%---------------------------------------------------------------
%-spec template()-> [{atom(),string()}].
%template()->
 %   gen_server:call(?SERVER, {template},infinity).


%% ====================================================================
%% Support functions
%
%%---------------------------------------------------------------
%% Function:read_state()
%% @doc: read theServer State variable      
%% @param: non 
%% @returns:State
%%
%%---------------------------------------------------------------
-spec read_state()-> term().
read_state()->
    gen_server:call(?SERVER, {read_state},infinity).
%% 
%% @doc:check if service is running
%% @param: non
%% @returns:{pong,node,module}|{badrpc,Reason}
%%
-spec ping()-> {atom(),node(),module()}|{atom(),term()}.
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).

%% ====================================================================
%% Gen Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
 %% Start needed applications
    ok=application:start(nodelog),
      %% Init logging 
    LogDir="logs",
    LogFileName="cluster.log",
    ok=file:make_dir(LogDir),
    LogFile=filename:join([LogDir,LogFileName]),
    nodelog_server:create(LogFile),    

    ok=application:start(sd),
    ok=application:start(node),
    ok=application:start(config),
    ok=cluster_lib:init_etcd(),

%    {ok,CookieStr}=db_deployments:read(cookie,DeploymentName),
%    true=erlang:set_cookie(node(),list_to_atom(CookieStr)),

    %% Deployments info
 %   {ok,DeploymentNameAtom}=application:get_env(deployment_name),
  %  DeploymentName=atom_to_list(DeploymentNameAtom),  
  %  {ok,ClusterId}=db_deployments:read(name,DeploymentName),
  
  %  {ok,Hosts}=db_deployments:read(hosts,DeploymentName),
      
  
    
   % Create k3 nodes at hosts
 %   NodeName=ClusterId++"_"++"node",
  %  PaArgs=" ",
  %  EnvArgs=" ",
  %  Appl="k3.spec",
  %  NodeDir=ClusterId,
%    StartedHostNodes=cluster_lib:start_host_nodes(Hosts,NodeName,CookieStr,PaArgs,EnvArgs,NodeAppl,NodeDir,DeploymentName),
%    [HostName|_]=Hosts,
    HostStartResult=rpc:call(node(),k3_remote_host,start_k3,[],25*5000),
    timer:sleep(1000),
    rpc:cast(node(),nodelog_server,log,[notice,?MODULE_STRING,?LINE,
						       {"HostStartResult",HostStartResult}]),
    
	
    
    {ok, #state{
	    start_time={date(),time()}
	   }
    }.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call({delete_cluster},_From, State) ->
    Reply=ok,
    NewState=State#state{
	       
	       start_time=undefined},
    {reply, Reply, NewState};

handle_call({read_state},_From, State) ->
    Reply=State,
    {reply, Reply, State};

handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call({stopped},_From, State) ->
    Reply=ok,
    {reply, Reply, State};


handle_call({not_implemented},_From, State) ->
    Reply=not_implemented,
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    %rpc:cast(node(),log,log,[?Log_ticket("unmatched call",[Request, From])]),
    Reply = {ticket,"unmatched call",Request, From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_cast(_Msg, State) ->
  %  rpc:cast(node(),log,log,[?Log_ticket("unmatched cast",[Msg])]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({nodedown,Node}, State) ->
    io:format(" ~p~n",[{?MODULE,?LINE,nodedown,Node}]),
    {noreply, State};

handle_info(Info, State) ->
    io:format("Info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

		  
