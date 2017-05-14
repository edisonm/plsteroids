:- use_module(library(doc_http)).
:- doc_server(4000).
:- [loadall].
:- working_directory(W,W),
   directory_file_path(W, 'index.html', Request),
   format(string(URL), 'http://localhost:~w/doc~w', [4000, Request]),
   www_open_url(URL).
