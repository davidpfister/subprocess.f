<a id="readme-top"></a>

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]

<!-- PROJECT LOGO -->
<br />
<div align="center">
  <h3 align="center">subprocess.f</h3>

  <p align="center">
    A experimental library for forking a child process in Fortran
    <br />
    <a href="https://github.com/davidpfister/subprocess.f"><strong>Explore the project »</strong></a>
    <br />
  </p>
</div>



<!-- TABLE OF CONTENTS -->
[TOC]

# Introduction
<!-- ABOUT THE PROJECT -->
## About the Project
<p align="center">
  <img src="https://github.com/davidpfister/subprocess.f/blob/master/.dox/images/image.png?raw=true">
</p>

This project aims at providing some ideas to create child processes in Fortran. <br><br>
It is quite experimental at the moment, but looks promising. <br>
The development of this repo is link to the discussion on the Fortran discourse about the [Stdlib system interaction API](https://fortran-lang.discourse.group/t/stdlib-system-interaction-api-call-for-feedback/9037)

* [![fpm][fpm]][fpm-url]
* [![ifort][ifort]][ifort-url]
* [![gfortran][gfortran]][gfortran-url]

<!-- GETTING STARTED -->
## Getting Started

### Requirements

To build that library you need

- a Fortran 2008 compliant compiler, or better, a Fortran 2018 compliant compiler.

The following compilers are tested on the default branch of _subprocess.f_:
<center>

| Name |	Version	| Platform	| Architecture |
|:--:|:--:|:--:|:--:|
| GCC Fortran (MinGW) | 14 | Windows 10 | x86_64 |
| Intel oneAPI classic	| 2021.5	| Windows 10 |	x86_64 |
| Intel oneAPI classic	| 2021.13	| Windows 10 |	x86_64 |

</center>

- a preprocessor. The units tests of _subprocess.f_ use quite some preprocessor macros. It is known to work both with intel `fpp` and `cpp`.  
Unit test rely on the the files [`assertion.inc`](https://github.com/davidpfister/fortiche/tree/master/src/assertion) and [`app.inc`](https://github.com/davidpfister/fortiche/tree/master/src/app). 

### Installation

#### Get the code
```bash
git clone https://github.com/davidpfister/subprocess.f
cd subprocess.f
```

#### Build with fpm

The repo is compatible with fpm projects. It can be build using _fpm_
```bash
fpm build
```

Building with ifort requires to specify the compiler name (gfortran by default)
```cmd
fpm build --compiler ifort
```
Alternatively, the compiler can be set using fpm environment variables.
```cmd
set FPM_FC=ifort
```

The toml files contains the settings to the cpp preprocessor are specified in the file. 

```toml
[preprocess]
cpp.suffixes = ["F90", "f90"]
cpp.macros = ["_FPM, _WIN32"]
```
The `_FPM` macro is used to differentiate the build when compiling with _fpm_ or _Visual Studio_. This is mostly present to adapt the hard coded paths that differs in both cases.
The `_WIN32` macro is used only on Windows system. It should be removed otherwise

#### Build with Visual Studio 2019

The project was originally developed on Windows with Visual Studio 2019. The repo contains the solution file (_subprocess.f.sln_) to get you started with Visual Studio 2019. 


<!-- USAGE EXAMPLES -->
## Usage


```fortran
use subprocess, only: process

type(process) :: p
logical :: succ
integer :: code

p = process('gfortran')
call p%run('hello_world.f90 -o hello_world')
```
<!-- CONTRIBUTING -->
### Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**. So, thank you for considering contributing to _subprocess.f_.
Please review and follow these guidelines to make the contribution process simple and effective for all involved. In return, the developers will help address your problem, evaluate changes, and guide you through your pull requests.

By contributing to _subprocess.f_, you certify that you own or are allowed to share the content of your contribution under the same license.

### Style

Please follow the style used in this repository for any Fortran code that you contribute. This allows focusing on substance rather than style.

### Reporting a bug

A bug is a *demonstrable problem* caused by the code in this repository.
Good bug reports are extremely valuable to us—thank you!

Before opening a bug report:

1. Check if the issue has already been reported
   ([issues](https://github.com/davidpfister/subprocess.f/issues)).
2. Check if it is still an issue or it has been fixed?
   Try to reproduce it with the latest version from the default branch.
3. Isolate the problem and create a minimal test case.

A good bug report should include all information needed to reproduce the bug.
Please be as detailed as possible:

1. Which version of _subprocess.f_ are you using? Please be specific.
2. What are the steps to reproduce the issue?
3. What is the expected outcome?
4. What happens instead?

This information will help the developers diagnose the issue quickly and with
minimal back-and-forth.

### Pull request

If you have a suggestion that would make this project better, please create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!
1. Open a [new issue](https://github.com/davidpfister/subprocess.f/issues/new) to
   describe a bug or propose a new feature.
   Refer to the earlier sections on how to write a good bug report or feature    request.
2. Discuss with the developers and reach consensus about what should be done about the bug or feature request.
   **When actively working on code towards a PR, please assign yourself to the
   issue on GitHub.**
   This is good collaborative practice to avoid duplicated effort and also inform others what you are currently working on.
3. Create your Feature Branch (```git checkout -b feature/AmazingFeature```)
4. Commit your Changes (```git commit -m 'Add some AmazingFeature'```)
5. Push to the Branch (```git push origin feature/AmazingFeature```)
6. Open a Pull Request with your contribution.
   The body of the PR should at least include a bullet-point summary of the
   changes, and a detailed description is encouraged.
   If the PR completely addresses the issue you opened in step 1, include in
   the PR description the following line: ```Fixes #<issue-number>```. If your PR implements a feature that adds or changes the behavior of _subprocess.f_,
   your PR must also include appropriate changes to the documentation and associated units tests.

In brief, 
* A PR should implement *only one* feature or bug fix.
* Do not commit changes to files that are irrelevant to your feature or bug fix.
* Smaller PRs are better than large PRs, and will lead to a shorter review and
  merge cycle
* Add tests for your feature or bug fix to be sure that it stays functional and useful
* Be open to constructive criticism and requests for improving your code.


<!-- LICENSE -->
## License

Distributed under the MIT License.

<!-- MARKDOWN LINKS & IMAGES -->
[contributors-shield]: https://img.shields.io/github/contributors/davidpfister/subprocess.f.svg?style=for-the-badge
[contributors-url]: https://github.com/davidpfister/subprocess.f/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/davidpfister/subprocess.f.svg?style=for-the-badge
[forks-url]: https://github.com/davidpfister/subprocess.f/network/members
[stars-shield]: https://img.shields.io/github/stars/davidpfister/subprocess.f.svg?style=for-the-badge
[stars-url]: https://github.com/davidpfister/subprocess.f/stargazers
[issues-shield]: https://img.shields.io/github/issues/davidpfister/subprocess.f.svg?style=for-the-badge
[issues-url]: https://github.com/davidpfister/subprocess.f/issues
[license-shield]: https://img.shields.io/github/license/davidpfister/subprocess.f.svg?style=for-the-badge
[license-url]: https://github.com/davidpfister/subprocess.f/master/LICENSE
[gfortran]: https://img.shields.io/badge/gfortran-000000?style=for-the-badge&logo=gnu&logoColor=white
[gfortran-url]: https://gcc.gnu.org/wiki/GFortran
[ifort]: https://img.shields.io/badge/ifort-000000?style=for-the-badge&logo=Intel&logoColor=61DAFB
[ifort-url]: https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html
[fpm]: https://img.shields.io/badge/fpm-000000?style=for-the-badge&logo=Fortran&logoColor=734F96
[fpm-url]: https://fpm.fortran-lang.org/
