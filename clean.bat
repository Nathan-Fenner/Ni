@echo off
rm *.hi
rm *.o
rm *.exe

ghc -Wall main.hs

rm *.hi
rm *.o