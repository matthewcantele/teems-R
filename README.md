# TEEMS R package

[![License](https://img.shields.io/badge/License-GPL-blue.svg)](LICENSE)
[![Version](https://img.shields.io/badge/version-0.9-green.svg)](https://github.com/username/repo/releases)


Currently under development!
## Installation
Install R if it is not yet on your system and an IDE like RStudio for interactive code execution
```bash
sudo apt intall r-base
```

Install the R package {remotes} and any dependencies flagged
```R
install.packages("remotes")
```

Install the latest release "v0.0.0.94" using {remotes}
```R
remotes::install_github("matthewcantele/teems-R@v0.0.0.94")
```
The installation may fail, citing "there is no package called ...". Install those packages and try again.

Sample scripts are located at the teems-scripts repository
```bash



## License

This project is licensed under the GPLv3.0 License - see the [LICENSE](LICENSE) file for details.

## Code authorship
This work is the culmination of many years of efforts and collaborations. The C source code (src) and main build was written by Tom Kompas and Ha Van Pham. The binary parsing code (bin_parser) was contributed by Martin Ingrahm. Finally, Matthew Cantele authored the Docker and Singularity scripts.

## Contact

- Project Maintainer: [Matthew Cantele](mailto:matthew.cantele@protonmail.com)
- Project Homepage: [https://github.com/matthewcantele/teems-solver](https://github.com/matthewcantele/teems-solver)
- Bug Reports: [https://github.com/username/repository/issues](https://github.com/username/repository/issues)