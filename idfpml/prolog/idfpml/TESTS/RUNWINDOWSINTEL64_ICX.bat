echo "BEGIN TESTS IN WINDOWS..."
del readtest.exe readtest.obj ..\LIBRARY\libbid.lib
call windowsbuild_icx.bat WIN_IA64
del readtest.exe readtest.obj ..\LIBRARY\libbid.lib
echo "END TESTS IN WINDOWS..."
echo "THE TESTS PASSED IF NO FAILURES WERE REPORTED ABOVE"
