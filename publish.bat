dotnet publish -r win10-x64 -c Release /p:PublishSingleFile=true /p:PublishTrimmed=true 
echo "output should be in bin\Release\netcoreapp3.0\win10-x64\publish"