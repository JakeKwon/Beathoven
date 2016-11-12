# Beathoven

## Installation under OS X

1. Install Homebrew:
```
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```
2. Install and set up opam:
```
brew install opam
opam init
```
3. Install llvm:
```
brew install homebrew/versions/llvm38
```
4. Have opam set up your enviroment:
```
eval `opam config env`
```
5. Install the OCaml llvm library:
```
opam install llvm.3.8
```
6. Create a symbolic link to the lli command:
```
sudo ln -s /usr/local/opt/llvm38/bin/lli-3.8 /usr/bin/lli
```

### Run Tests (All)
To make sure everything is working, navigate to the root directory of QL and run the following command:
```bash
make
cd test
make
```
The outputs should be generated.

#### Run Scanner Tests
```bash
cd test
./scanner_test.sh
```

#### Run Pass Tests
```bash
cd test
./pass_test.sh
```

#### Run Fail Tests
```bash
cd test
./fail_test.sh
```
