# TEEMS R package

[![License](https://img.shields.io/badge/License-GPL-blue.svg)](LICENSE)
[![Version](https://img.shields.io/badge/version-0.9-green.svg)](https://github.com/username/repo/releases)



Clone the repository to a local directory
For Tom:
```bash
git clone https://tomkompas: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@github.com/matthewcantele/teems-R.git
```

For testing:
```bash
git clone https://mcantele_testing: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@github.com/matthewcantele/teems-R.git
```

You need R if you haven't installed it yet
```bash
sudo apt intall r-base
```

Some necessary packages for this beta stage
```bash
R -e 'install.packages(c("devtools", "data.table"))'
```

Enter directory
```bash
cd teems-R
```



## License

This project is licensed under the GPLv3.0 License - see the [LICENSE](LICENSE) file for details.

## Code authorship
This work is the culmination of many years of efforts and collaborations. The C source code (src) and main build was written by Tom Kompas and Ha Van Pham. The binary parsing code (bin_parser) was contributed by Martin Ingrahm. Finally, Matthew Cantele authored the Docker and Singularity scripts.

## Contact

- Project Maintainer: [Matthew Cantele](mailto:matthew.cantele@protonmail.com)
- Project Homepage: [https://github.com/matthewcantele/teems-solver](https://github.com/matthewcantele/teems-solver)
- Bug Reports: [https://github.com/username/repository/issues](https://github.com/username/repository/issues)