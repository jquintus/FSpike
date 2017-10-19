var target = Argument("target", "Default");

var slnFile = @".\FSpike.sln";
var configuration = BuildSystem.IsRunningOnAppVeyor
    ? "Release"
    : "Debug"; 

Task("Clean")
    .Does(() =>
{
    DotNetBuild(slnFile, settings => settings
	.SetConfiguration(configuration)
	.WithTarget("Clean") );
});

Task("Restore")
    .Does(() => 
{
    NuGetRestore(slnFile);
});

Task("Build")
    .IsDependentOn("Clean")
    .IsDependentOn("Restore")
    .Does(() =>
{
    DotNetBuild(slnFile, settings => settings
        .SetConfiguration(configuration));
});

Task("Test")
    .IsDependentOn("Build")
    .Does(() =>
{
    var tests = @".\FSpike.Tests\bin\" + configuration + @"\FSpike.Tests.exe";
    var exitCode = StartProcess(tests);
    if (exitCode > 0) throw new Exception("Tests failed with exit code of " + exitCode);

});

Task("Default")
    .IsDependentOn("Test");

RunTarget(target);
