del *.obj *.lib
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib clang000libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib clang001libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib clang010libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib clang011libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib clang100libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib clang101libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib clang110libbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
ren libbid.lib clang111libbid.lib
make clean 


make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib clang000blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib clang001blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib clang010blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib clang011blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib clang100blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib clang101blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib clang110blibbid.lib
make clean 
make _HOST_OS=Windows_NT  CC=clang CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
ren libbid.lib clang111blibbid.lib
make clean 
