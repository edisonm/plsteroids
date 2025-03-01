echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 000 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl000libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 001 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl001libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 010 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl010libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 011 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl011libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 100 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl100libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 101 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl101libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 110 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl110libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 111 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl111libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
del ..\LIBRARY\libbid.lib



echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 000b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl000blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 001b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl001blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 010b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl010blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 011b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl011blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 100b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl100blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 101b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl101blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 110b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl110blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR cl 111b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\cl111blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=cl CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
del ..\LIBRARY\libbid.lib
