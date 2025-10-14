#!/bin/bash
# Coverage Report Generator for FsMath

set -e

echo "🧪 Running tests with coverage collection..."
./build.sh runtestswithcoverage

echo "📊 Installing ReportGenerator..."
dotnet tool install --global dotnet-reportgenerator-globaltool 2>/dev/null || echo "ReportGenerator already installed"

echo "📈 Generating coverage report..."
reportgenerator \
  -reports:"TestResults/**/coverage.cobertura.xml" \
  -targetdir:"TestResults/CoverageReport" \
  -reporttypes:"Html;Cobertura;JsonSummary;MarkdownSummary" \
  -verbosity:"Info"

echo "✅ Coverage report generated!"
echo "📂 Report location: TestResults/CoverageReport/index.html"
echo ""
echo "🌐 To view the report, open the HTML file in your browser:"
echo "   file://$(pwd)/TestResults/CoverageReport/index.html"