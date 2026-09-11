# Lazy compilation

## Build

Simply run the build script in `build.sh`

## Usage

Run `lcc` with:
- `-F` the C files directory, containing the files to compile;
- `-C` the configuration file;
- `-B` the binary name to generate (optional if you only want to compile a few files).

Five environment variables can also be defined:

- `OUTPUT_DIR` is the directory in which the .o files will be generated (default: `"output"`);
- `DEPGRAPH_FILENAME` is the file in which the serialized dependency graph will be stored (this file is used to store compilation results) (default: `".depgraph"`);
- `DEBUG` is a flag to display debug messages (default: `0`);
- `PEDANTIC` is a flag to make the compiler pedantic (default: `1`);
- `CC` is the compiler to use (default: `gcc`)

### Configuration file

The configuration file starts with `#External dependencies` and is followed
by a list of lines with the following format:

```
<file>:<command>
```

where: 

- the file corresponds to an external dependency of the project to compile;
- the command will be run to get the version of the dependency (if unneccesary, just use `echo ""`).

You can find a config file example, `config`, at the root of the current directory.
