dotnet publish -r linux-x64 -c Release /p:PublishSingleFile=true /p:PublishTrimmed=true 
echo "output should be in bin\Release\netcoreapp3.0\linux-x64\publish"