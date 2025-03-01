echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 000 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang000libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 001 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang001libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 010 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang010libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 011 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang011libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 100 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang100libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 101 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang101libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 110 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang110libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 111 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang111libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
del ..\LIBRARY\libbid.lib



echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 000b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang000blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 001b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang001blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 010b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang010blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 011b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang011blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 100b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang100blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 101b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang101blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 110b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang110blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR clang 111b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\clang111blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=clang CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
del ..\LIBRARY\libbid.lib
