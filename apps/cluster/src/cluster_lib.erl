%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(cluster_lib).   
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).
-export([
	 init_etcd/0,
	 start_host_nodes/8,
	 
	 start_k3_on_hosts/3,

	 
	 create_cluster/5
	]).
	 

%% ====================================================================
%% External functions
%% ====================================================================

start_host_nodes(Hosts,NodeName,CookieStr,PaArgs,EnvArgs,NodeAppl,NodeDir,DeploymentName)->
    start_host_nodes(Hosts,NodeName,CookieStr,PaArgs,EnvArgs,NodeAppl,NodeDir,DeploymentName,[]).
    
start_host_nodes([],_NodeName,_CookieStr,_PaArgs,_EnvArgs,_NodeAppl,_NodeDir,DeploymentName,StartedNodes)->
    StartedNodes;
start_host_nodes([HostName|T],NodeName,CookieStr,PaArgs,EnvArgs,NodeAppl,NodeDir,DeploymentName,Acc)->
    NewAcc=case node_server:ssh_create(HostName,NodeName,CookieStr,PaArgs,EnvArgs) of
	       {error,Reason}->     
		   rpc:cast(node(),nodelog_server,log,[warning,?MODULE_STRING,?LINE,
						       {error,HostName,NodeName,Reason}]),
		   Acc;
	       {ok,Node}->
		   rpc:cast(node(),nodelog_server,log,[info,?MODULE_STRING,?LINE,
						       {"succesful start node ",Node,HostName,NodeName}]),
		   rpc:call(Node,os,cmd,["rm -rf "++NodeDir],5000),
		   case rpc:call(Node,file,make_dir,[NodeDir],5000) of
		       {error,Reason}->
			   rpc:cast(node(),nodelog_server,log,[warning,?MODULE_STRING,?LINE,
						       {"failed create NodeDir ",Node,NodeDir,Reason}]),
			   Acc;
		       ok->
			   {ok,ApplVsn}=db_application_spec:read(vsn,NodeAppl),
			   {ok,GitPath}=db_application_spec:read(gitpath,NodeAppl),
			   {ok,StartCmd}=db_application_spec:read(cmd,NodeAppl),
			   ok=rpc:call(Node,application,set_env,[[{k3,[{deployment_name,DeploymentName}]}]],5000),
			   case node_server:load_start_appl(Node,NodeDir,NodeAppl,ApplVsn,GitPath,StartCmd) of
			       {error,Reason}->
				   rpc:cast(node(),nodelog_server,log,[warning,?MODULE_STRING,?LINE,
								       {"failed create NodeDir ",Node,NodeDir,Reason}]),
				   Acc;
			       {ok,ApplId,ApplVsn,ApplDir}->
				   rpc:cast(node(),nodelog_server,log,[info,?MODULE_STRING,?LINE,
								       {"succesful start application ",Node,ApplId,ApplDir}]),
				   [Node|Acc]
			   end
		   end
	   end,
    start_host_nodes(T,NodeName,CookieStr,PaArgs,EnvArgs,NodeAppl,NodeDir,NewAcc).
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
init_etcd()->
    ok=etcd_server:appl_start([]),
    pong=etcd_server:ping(), 
    ok=etcd_server:dynamic_db_init([]),
    ok=db_host_spec:init_table(),
    ok=db_application_spec:init_table(),
    ok=db_deployment_info:init_table(),
    ok=db_deployments:init_table(),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_k3_on_hosts(ClusterId,CookieStr,Hosts)->
    
    NodeName=ClusterId++"_k3",
    {ok,MyHostName}=net:gethostname(),
    AllHostNames=[HostName||HostName<-Hosts,
			    MyHostName/=HostName],
						      
    PaArgs=" ",
    EnvArgs=" ",
    ClusterDir=ClusterId,
    AllK3Nodes=start_host_vm(AllHostNames,ClusterDir,NodeName,CookieStr,PaArgs,EnvArgs,[]),
 %   {ok,HostNode,HostName}
    
    %% start node on each K3
    ok=git_load_start_node(AllK3Nodes),
    AllK3Nodes.



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
git_load_start_node([])->
    ok;
git_load_start_node([{ok,Node,_HostName,ClusterDir}|T])->
    %% copy host_info_specs and deployments
    ok=rpc:call(Node,file,make_dir,[filename:join([ClusterDir,"host_info_specs"])],5000),
    rpc:call(Node,os,cmd,["cp host_info_specs/* "++filename:join([ClusterDir,"host_info_specs"])]),
    ok=rpc:call(Node,file,make_dir,[filename:join([ClusterDir,"deployments"])],5000),
    rpc:call(Node,os,cmd,["cp host_info_specs/* "++filename:join([ClusterDir,"deployments"])]),

    %% git clone application and deployment specs
    _GitPathApplicationInfo=config_server:deployment_spec_applicatioapplication_gitpath("node.spec"),


    ApplDir=filename:join([ClusterDir,"node"]),
    ok=rpc:call(Node,file,make_dir,[ApplDir],5000),
    GitPath=config_server:application_gitpath("node.spec"),
    {M,F,A}=config_server:application_start_cmd("node.spec"),
   
    TempDir="temp"++"_"++ClusterDir++".dir",
    rpc:call(Node,os,cmd,["rm -rf "++TempDir],5000),
    ok=rpc:call(Node,file,make_dir,[TempDir],5000),
    rpc:call(Node,os,cmd,["git clone "++GitPath++" "++TempDir],5000),
    rpc:call(Node,os,cmd,["mv  "++TempDir++"/*"++" "++ApplDir],5000),
    rpc:call(Node,os,cmd,["rm -rf "++TempDir],5000),
    Ebin=filename:join([ApplDir,"ebin"]),
    true= rpc:call(Node,filelib,is_dir,[Ebin],5000),
    true=rpc:call(Node,code,add_patha,[Ebin],5000),
    true=rpc:call(Node,code,add_patha,[ClusterDir],5000),
    true=rpc:call(Node,code,add_patha,[ClusterDir++"/*"],5000),
    ok=rpc:call(Node,M,F,A,2*5000),
    pong=rpc:call(Node,node_server,ping,[],5000),
    git_load_start_node(T).


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

start_host_vm([],_ClusterDir,_NodeName,_CookieStr,_PaArgs,_EnvArgs,AllK3Nodes)->
    AllK3Nodes;
start_host_vm([HostName|T],ClusterDir,NodeName,CookieStr,PaArgs,EnvArgs,Acc)->
    NewAcc=case node_server:ssh_create(HostName,NodeName,CookieStr,PaArgs,EnvArgs) of
	       {ok,HostNode}->
		   case rpc:call(HostNode,os,cmd,["rm -rf "++ClusterDir],5000) of
		       {badrpc,Reason}->
			   nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Error reate node at host ",HostName,badrpc,Reason}),
			   Acc;
		       _->
			   case rpc:call(HostNode,file,make_dir,[ClusterDir],5000) of
			       {error,Reason}->
				   nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Error failed to create node at host ",HostName,Reason}),
				   Acc;
			       ok->
				   nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Cluster node  successfully created at host ",HostName," ",HostNode}),
				   [{ok,HostNode,HostName,ClusterDir}|Acc]
			   end
		   end;
	       {error,Reason}->
		   nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Error failed to create node at host ",HostName,Reason}),
		   Acc;
	       Error ->
		   nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Error reate node at host ",HostName,Error}),
		   Acc
	   end,
    start_host_vm(T,ClusterDir,NodeName,CookieStr,PaArgs,EnvArgs,NewAcc).
    
		   
		   


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------





%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create_cluster(ClusterName,Cookie,NumControllers,NumWorkers,_K3Nodes)->
    os:cmd("rm -rf "++ClusterName),
    Reply=case file:make_dir(ClusterName) of
	      {error,Reason}->
		  {error,Reason};
	      ok->
		  
		  {StartedControllers,FailedControllers}=create(NumControllers,ClusterName,Cookie,"controller",[],[]),
		  {StartedWorkers,FailedWorkers}=create(NumWorkers,ClusterName,Cookie,"worker",[],[]),
	
		  {ok,[{controllers,{StartedControllers,FailedControllers}},
		       {workers,{StartedWorkers,FailedWorkers}}]}
	
	  end,
    Reply.
			  
create(0,_,_,_,Started,Failed)->	  
    {Started,Failed};
create(N,ClusterName,Cookie,Type,Started,_Failed)->
    UniqueString=integer_to_list(erlang:system_time(microsecond),36),
    NodeName=ClusterName++"_"++Type++integer_to_list(N)++"_"++UniqueString,
    NodeDir=filename:join(ClusterName,NodeName),
    ok=file:make_dir(NodeDir),
    {ok,HostName}=net:gethostname(),
    PaArgs=" ",
    EnvArgs=" ",
    
    {ok,Node}=node_server:create(HostName,NodeDir,NodeName,Cookie,PaArgs,EnvArgs),
    true=erlang:monitor_node(Node,true),
    
    %% start commonm
    GitPathCommon=config_server:application_gitpath("common.spec"),
    StartCmdCommon=config_server:application_start_cmd("common.spec"),
    {ok,"common","0.1.0",_ApplDirCommon}=node_server:load_start_appl(Node,NodeDir,"common","0.1.0",GitPathCommon,StartCmdCommon),
    %% start nodelog
    GitPathNodelog=config_server:application_gitpath("nodelog.spec"),
    StartCmdNodelog=config_server:application_start_cmd("nodelog.spec"),
    {ok,"nodelog","0.1.0",_ApplDirNodelog}=node_server:load_start_appl(Node,NodeDir,"nodelog","0.1.0",GitPathNodelog,StartCmdNodelog),
    ok=file:make_dir(filename:join(NodeDir,"logs")),
    LogFile=filename:join([NodeDir,"logs",NodeName++".log"]),
    rpc:call(Node,nodelog_server,create,[LogFile],5000),

    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Started common"}),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Started nodelog"}),
   %% start sd
    GitPathSd=config_server:application_gitpath("sd.spec"),
    StartCmdSd=config_server:application_start_cmd("sd.spec"),
    {ok,"sd","0.1.0",_ApplDirSd}=node_server:load_start_appl(Node,NodeDir,"sd","0.1.0",GitPathSd,StartCmdSd),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Started sd"}),	 
    NewStarted=[[{type,list_to_atom(Type)},{node_name,NodeName},{node_dir,NodeDir},{node,Node},{created,{date(),time()}}]|Started],
    NewFailed=[],
    create(N-1,ClusterName,Cookie,Type,NewStarted,NewFailed).



