del *.obj *.lib
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icl000libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icl001libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icl010libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icl011libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icl100libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icl101libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icl110libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icl111libbid.lib
make clean 


make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icl000blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icl001blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icl010blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icl011blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icl100blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icl101blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icl110blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icl CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icl111blibbid.lib
make clean 
