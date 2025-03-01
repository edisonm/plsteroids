echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 000 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx000libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 001 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx001libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 010 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx010libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 011 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx011libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 100 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx100libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 101 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx101libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 110 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx110libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 111 ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx111libbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
readtest < readtest.in
del ..\LIBRARY\libbid.lib



echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 000b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx000blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 001b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx001blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 010b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx010blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 011b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx011blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=0 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 100b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx100blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 101b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx101blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=1 GLOBAL_RND=0 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 110b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx110blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
echo ""
echo ""
echo "****************** RUNNING TESTS FOR icx 111b ***************************"
echo ""
echo ""
del readtest.exe readtest.obj
copy /Y  ..\LIBRARY\icx111blibbid.lib ..\LIBRARY\libbid.lib
make OS_TYPE=%1 CC=icx CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=1
readtest < readtest.in
del ..\LIBRARY\libbid.lib
