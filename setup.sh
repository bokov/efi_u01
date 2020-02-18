#!/bin/bash

git init;
./initgitmods.sh;
git add .gitignore .Rprofile data *.R *.sh README.md LICENSE
git ci -a -m "First commit";
git remote add origin git@github.com:bokov/efi_u01.git
git push -u origin master;
