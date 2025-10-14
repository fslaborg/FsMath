# Coverage Report Generator for FsMath
# PowerShell script for Windows

Write-Host "🧪 Running tests with coverage collection..." -ForegroundColor Green
.\build.cmd runtestswithcoverage

Write-Host "📊 Installing ReportGenerator..." -ForegroundColor Yellow
try {
    dotnet tool install --global dotnet-reportgenerator-globaltool 2>$null
} catch {
    Write-Host "ReportGenerator already installed" -ForegroundColor Yellow
}

Write-Host "📈 Generating coverage report..." -ForegroundColor Green
& reportgenerator `
  -reports:"TestResults/**/coverage.cobertura.xml" `
  -targetdir:"TestResults/CoverageReport" `
  -reporttypes:"Html;Cobertura;JsonSummary;MarkdownSummary" `
  -verbosity:"Info"

Write-Host "✅ Coverage report generated!" -ForegroundColor Green
Write-Host "📂 Report location: TestResults/CoverageReport/index.html" -ForegroundColor White
Write-Host ""
Write-Host "🌐 To view the report, open the HTML file in your browser:" -ForegroundColor Cyan
Write-Host "   file://$(Get-Location)/TestResults/CoverageReport/index.html" -ForegroundColor White