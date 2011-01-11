echo Cleanup ...
del /q ebin\*.beam ebin\*.dump

echo Building ...
cd src
FOR %%f IN (*.erl) DO erlc.exe -W -o ../ebin %%f
cd ..
