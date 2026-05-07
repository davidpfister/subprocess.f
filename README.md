<!--! @mainpage -->
<h1 class="title">
    <span class="name">Subprocess.f</span>
    <br>
    <span class="text">Spawning processes seamlessly</span>
    <br>
    <span class="tagline">
    A crossplatform library to create, wait and redirect child processes.
    </span>
</h1>
<br>

<div class="actions">
    <div class="action">
        <a class="button medium brand" href="#autotoc_md2">Get Started</a>
    </div>
    <div class="action">
        <a class="button medium alt" href="topics.html">API</a>
    </div>
    <div class="action">
        <a class="button medium alt" href="https://github.com/davidpfister/subprocess.f" target="_blank" rel="noreferrer">View on GitHub</a>
		  
    </div>
</div>


# Introduction
<!-- ABOUT THE PROJECT -->
## About the Project
<div style="text-align: center;">
  <img src="https://github.com/davidpfister/subprocess.f/blob/master/.dox/images/logo.png?raw=true" width="512" height="512">
</div>

This project aims at providing an easy and comprehensive API to spawn child processes in Fortran. <br>
The development of this repo is linked to the discussion on the Fortran discourse about the [Stdlib system interaction API](https://fortran-lang.discourse.group/t/stdlib-system-interaction-api-call-for-feedback/9037)

* [![fpm][fpm]][fpm-url]
* [![ifort][ifort]][ifort-url]
* [![gfortran][gfortran]][gfortran-url]

<!-- GETTING STARTED -->
## Installation

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

Besides the build command, several commands are also available:
```bash
@pretiffy
option clean --all
system codee format ./src
system fortitute check ./src --fix

@clean
option clean --all

@rebuild
system rmdir /s /q build
option build --flag '-ffree-line-length-none'

@build
option build --flag '-ffree-line-length-none'

@test
options test --flag '-ffree-line-length-none'

@doc
option clean --all
system cd ./.dox & doxygen ./Doxyfile.in & cd ..
```

The toml files contains the settings to the cpp preprocessor are specified in the file. 

```toml
[preprocess]
cpp.suffixes = ["F90", "f90"]
cpp.macros = ["_FPM, _WIN32"]
```
The `_FPM` macro is used to differentiate the build when compiling with _fpm_ or _Visual Studio_. This is mostly present to adapt the hard coded paths that differs in both cases.
The `_WIN32` macro is used only on Windows system. It should be removed otherwise.

#### Build with Visual Studio 2019

The project was originally developed on Windows with Visual Studio 2019. The repo contains the solution file (_subprocess.f.sln_) to get you started with Visual Studio 2019. 
<!-- USAGE EXAMPLES -->
## Quick Start

The easiest way to invoke a subprocess is to call the `run` subroutine. 

```fortran
type(process) :: p

p = process('gfortran')
call p%run('hello_world.f90 -o hello_world')
```

The library is written in both functional and oop style meaning that most subroutine can be invoked both as as classical subroutine or attached procedures. For instance, the previous example would write as follows:

```fortran
type(process) :: p

p = process('gfortran')
call run(p, 'hello_world.f90', '-o hello_world')
```

The process can also run asynchronously by invoking the `runasync` procedure.

```fortran
type(process) :: p

p = process('gfortran')
call runasync(p, 'hello_world.f90', '-o hello_world')
!...
call wait(p)
```

## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**. So, thank you for considering contributing to _benchmark.f_.
Please review and follow these [guidelines](https://github.com/davidpfister/subprocess.f/tree/master?tab=contributing-ov-file) to make the contribution process simple and effective for all involved. In return, the developers will help address your problem, evaluate changes, and guide you through your pull requests.

By contributing to _benchmark.f_, you certify that you own or are allowed to share the content of your contribution under the same license.
<!-- LICENSE -->
## License

Distributed under the MIT License.

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]

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
