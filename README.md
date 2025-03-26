# FsMath

## build

Check the [build project](./build) to take a look at the build targets.

```shell
# Windows
./build.cmd

# Linux/mac
build.sh
```

## running and writing tests

```shell
# Windows
./build.cmd runtests

# Linux/mac
build.sh runtests
```

## docs

The docs are contained in `.fsx` and `.md` files in the `docs` folder. To develop docs on a local server with hot reload, run the following in the root of the project:

```shell
# Windows
./build.cmd watchdocs

# Linux/mac
./build.sh watchdocs
```

## pack nuget package(s)
```shell
# Windows
./build.cmd pack

# Linux/mac
build.sh pack
```