# h2decays

## Dependencies

In order to build from the source, the [GSL](https://www.gnu.org/software/gsl/) interfacing with [LAPACK](http://www.netlib.org/lapack/) and [LHAPDF](https://lhapdf.hepforge.org/) must be installed.

```
$ gsl-config --version
2.6

$ lhapdf-config --version
6.2.3
```

In [Arch Linux](https://archlinux.org/), you can install them via

```
sudo pacman -S gsl lapack lhapdf
```

See [`default.nix`](./default.nix) and [`release.nix`](./release.nix) for building with [Nix](https://nixos.org/nix/).

## Usage

```
$ h2decays --help
Calculate the branching ratio of heavy Higgs boson

Usage: h2decays [--mtype INT] [--mH DOUBLE]... --mHp DOUBLE [--mS DOUBLE]
                --tanb DOUBLE --cosba DOUBLE [--stepsize DOUBLE]
                [--output STRING]

Available options:
  -h,--help                Show this help text
  --mtype INT              model type (either 1 or 2)
  --mH DOUBLE...           heavy Higgs mass
  --mHp DOUBLE             charged Higgs mass
  --mS DOUBLE              heavy mass scale (m_A if MSSM)
  --tanb DOUBLE            tan(beta)
  --cosba DOUBLE           cos(beta-alpha)
  --stepsize DOUBLE        step size (default: 0.5)
  --output STRING          the name of the output file

$ hpdecays --help
Calculate the branching ratio of charged Higgs boson

Usage: hpdecays [--mtype INT] [--mHp DOUBLE]... --tanb DOUBLE --cosba DOUBLE
                [--stepsize DOUBLE] [--output STRING]

Available options:
  -h,--help                Show this help text
  --mtype INT              model type (either 1 or 2)
  --mHp DOUBLE...          charged Higgs mass
  --tanb DOUBLE            tan(beta)
  --cosba DOUBLE           cos(beta-alpha)
  --stepsize DOUBLE        step size (default: 0.5)
  --output STRING          the name of the output file
```
