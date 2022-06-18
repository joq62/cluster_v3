%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(remote_host).   
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).
-export([
	 start/8
	]).
	 

%% ====================================================================
%% External functions
%% ====================================================================

start(HostName,NodeName,CookieStr,PaArgs,EnvArgs,_Appl,NodeDirBase,_DeploymentName)->


    {ok,Node,NodeDir}=create_node(HostName,NodeName,CookieStr,PaArgs,EnvArgs,NodeDirBase),
    ok=etcd_init(Node,NodeDir),
    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create_node(HostName,NodeName,CookieStr,PaArgs,EnvArgs,NodeDirBase)->
    {ok,Node}=node_server:ssh_create(HostName,NodeName,CookieStr,PaArgs,EnvArgs),
    {ok,Cwd}=rpc:call(Node,file,get_cwd,[],5000),
    NodeDir=filename:join(Cwd,NodeDirBase),
    []=rpc:call(Node,os,cmd,["rm -rf "++NodeDir],5000),
    timer:sleep(2000),
    ok=rpc:call(Node,file,make_dir,[NodeDir],5000),
    rpc:cast(node(),nodelog_server,log,[info,?MODULE_STRING,?LINE,
						       {"succesful created node at host",Node,HostName}]),
    {ok,Node,NodeDir}.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
etcd_init(Node,NodeDir)->
    NodeAppl="etcd.spec",
    {ok,ApplId}=db_application_spec:read(name,NodeAppl),
    {ok,ApplVsn}=db_application_spec:read(vsn,NodeAppl),
    {ok,GitPath}=db_application_spec:read(gitpath,NodeAppl),
    {ok,StartCmd}=db_application_spec:read(cmd,NodeAppl),

    {ok,"etcd.spec",_,_}=node_server:load_start_appl(Node,NodeDir,ApplId,ApplVsn,GitPath,StartCmd),
    pong=rpc:call(Node,etcd_server,ping,[],5000),
    ok=rpc:call(Node,etcd_server,dynamic_db_init,[[node()]],5000),
    %io:format("mnesia ~p~n",[rpc:call(Node,mnesia,system_info,[],5000)]),
    rpc:cast(node(),nodelog_server,log,[info,?MODULE_STRING,?LINE,
						       {"succesful start etcd at  node ",Node}]),
    
    ok.
    
    
    

    

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
