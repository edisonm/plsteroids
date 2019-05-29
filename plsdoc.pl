:- use_module(library(doc_http)).
:- doc_server(4000).
:- [loadall].
:- format(string(URL), 'http://localhost:~w/pldoc/doc/_CWD_/index.html', [4000]),
   www_open_url(URL).
