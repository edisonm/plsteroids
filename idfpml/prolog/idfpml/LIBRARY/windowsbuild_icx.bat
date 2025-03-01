del *.obj *.lib
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icx000libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icx001libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icx010libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icx011libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icx100libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icx101libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icx110libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib icx111libbid.lib
make clean 


make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icx000blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icx001blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icx010blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icx011blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icx100blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icx101blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icx110blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=icx CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib icx111blibbid.lib
make clean 
