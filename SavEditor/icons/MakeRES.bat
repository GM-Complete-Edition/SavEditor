SET mypath=%~dp0
echo %mypath:~0,-1%
cd %mypath:~0,-1%
for %%a in (*.rc) do (
BRCC32 %%a"
)