:- begin_tests(broker).

:- use_module(library(broker_rt)).
:- use_module(library(interface)).
:- use_module(broker_example_common).
:- use_module(broker_example_server).
:- use_module(broker_example_proxy).
:- use_module(broker_example_client).

test(broken1) :-
    start_wsbroker(3333),
    sleep(1),
    add_server(mylists,      'ws://localhost:3333/broker'),
    add_server(mylists_remt, 'ws://localhost:3333/broker'),
    bind_interface(mylists_intf, mylists_locl), % use the local implementation
    bect(W),
    bind_interface(mylists_intf, mylists_remt), % use the server
    bect(X),
    bind_interface(mylists_intf, mylists_prxy), % use the proxy, which will use the server
    bect(Y),
    assertion((W==X,X==Y,X==[1,2,3,4,5])).

:- end_tests(broker).
