# Coverage Analysis Setup

This document describes the code coverage analysis and reporting setup for FsMath.

## Overview

The project uses [Coverlet](https://github.com/coverlet-coverage/coverlet) for code coverage collection and reporting. Coverage reports are generated automatically during CI/CD builds with detailed HTML reports available as artifacts and coverage summaries in pull requests.

## Tools Used

- **Coverlet**: .NET code coverage library
- **ReportGenerator**: Tool for generating HTML/XML coverage reports
- **GitHub Actions**: CI/CD automation with coverage integration

## Local Development

### Running Tests with Coverage

```bash
# Basic coverage collection
./build.sh runtestswithcoverage   # Linux/macOS
./build.cmd runtestswithcoverage  # Windows

# Full coverage report generation
./scripts/generate-coverage.sh   # Linux/macOS
./scripts/generate-coverage.ps1  # Windows
```

### Coverage Configuration

The coverage setup is configured through several files:

- `coverlet.runsettings`: MSBuild test settings for coverage collection
- `.coverletrc`: Coverlet-specific configuration for exclusions
- Test project files include `coverlet.msbuild` and `coverlet.collector` packages

### Exclusions

The following items are excluded from coverage analysis:
- Test assemblies and test-related code
- Generated code (marked with attributes)
- Program entry points
- Obsolete code

## CI/CD Integration

### GitHub Actions Workflow

The `build-and-test.yml` workflow includes:

1. **Test Execution**: Runs tests with coverage collection using XPlat Code Coverage
2. **Report Generation**: Creates HTML and Cobertura reports using ReportGenerator
3. **PR Comments**: Adds coverage summary comments to pull requests
4. **Artifact Storage**: Stores HTML coverage reports as build artifacts

### Coverage Thresholds

- **Warning threshold**: 60% coverage
- **Good threshold**: 80% coverage
- **CI doesn't fail** on coverage below thresholds (informational only)

## Coverage Reports

### Local Reports

- **Location**: `TestResults/CoverageReport/`
- **Format**: HTML (main), Cobertura XML, JSON summary
- **View**: Open `TestResults/CoverageReport/index.html` in browser

### GitHub Integration

- **Coverage Badge**: Generated in pull request comments
- **PR Integration**: Coverage changes shown in pull request comments
- **Artifact Downloads**: Full HTML reports available from GitHub Actions

## Troubleshooting

### Common Issues

1. **No coverage data**: Ensure tests are running and `coverlet.collector` is installed
2. **Low coverage**: Check exclusions in `.coverletrc` and `coverlet.runsettings`
3. **Report generation fails**: Verify ReportGenerator is installed globally

### Debug Coverage Collection

```bash
# Run with verbose logging
dotnet test --collect:"XPlat Code Coverage" --logger "console;verbosity=detailed"

# Check for coverage files
find TestResults -name "*.cobertura.xml"
```

## Maintenance

The coverage setup should be reviewed periodically to:

- Update exclusion patterns as the codebase evolves
- Adjust coverage thresholds based on project maturity
- Update tool versions (Coverlet, ReportGenerator)
- Review GitHub Actions workflow and coverage reporting