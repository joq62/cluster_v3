all:
	rm -rf  *~ */*~ apps/cluster/src/*.beam test/*.beam erl_cra*;
	rm -rf  cluster* logs *.pod_dir rebar.lock;
	rm -rf _build test_ebin ebin
	rm -rf deployments temp *_info_specs;
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build test_ebin logs log;
	echo Done
create:
	rm -rf  *~ */*~ apps/cluster/src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs log *.pod_dir
	rm -rf deployments host_info_specs;
	rm -rf rebar.lock;
	rm -rf ebin;
#	host_info_specs dir and deployments dir shall be installed once
	mkdir  host_info_specs;
	cp ../../specifications/host_info_specs/*.host host_info_specs;
	git clone https://github.com/joq62/deployments.git;
	git clone https://github.com/joq62/application_info_specs.git;
	git clone https://github.com/joq62/deployment_info_specs.git;
#	Delete and create new cluster dir to make a clean start
	rebar3 compile;
	cp _build/default/lib/*/ebin/* ebin;
	erl -pa * -pa ebin -sname cluster -run cluster start;
	echo Done
eunit:
	rm -rf  *~ */*~ apps/cluster/src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs log *.pod_dir
	rm -rf deployments host_info_specs;
	rm -rf rebar.lock;
	rm -rf ebin;
#	host_info_specs dir and deployments dir shall be installed once
	rm -rf  *~ */*~ apps/k3/src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs log *.pod_dir
	rm -rf deployments *_info_specs;
	rm -rf rebar.lock;
	rm -rf ebin;
#	host_info_specs dir and deployments dir shall be installed once
	mkdir  host_info_specs;
	cp ../../specifications/host_info_specs/*.host host_info_specs;
	git clone https://github.com/joq62/deployments.git;
	git clone https://github.com/joq62/application_info_specs.git;
	git clone https://github.com/joq62/deployment_info_specs.git;
#	Delete and create new cluster dir to make a clean start
	mkdir ebin;
	erlc -o ebin /home/joq62/erlang/infra_2/k3_node/apps/k3_node/src/k3_node_remote_host.erl;
	rebar3 compile;
	cp _build/default/lib/*/ebin/* ebin;
#	testing
	mkdir test_ebin;
	erlc -o test_ebin test/*.erl;
#	erl -pa * -pa ebin -pa test_ebin -sname cluster_test -run basic_eunit start -cluster deployment_name $(deployment_name)
	erl -pa * -pa ebin -pa test_ebin -sname $(deployment_name) -run basic_eunit start -cluster deployment_name $(deployment_name)
