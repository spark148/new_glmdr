#! /bin/bash

## This script pulls the latex from overleaf.
## It may requires the credentials

git clone https://git.overleaf.com/5f57eef045d91700015607e9
rm -rf Overleaf
mv 5f57eef045d91700015607e9 Overleaf
