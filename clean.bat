@echo off
cls
rm *.hi
rm *.o
rm *.exe

ghc -Wall main.hs

rm *.hi
rm *.o