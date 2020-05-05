dotnet publish -r rhel.6-x64 -c Release /p:PublishSingleFile=true /p:PublishTrimmed=true 
echo "output should be in bin\Release\netcoreapp3.0\rhel.6-x64\publish"