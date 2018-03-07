#!/usr/bin/env bash

ghc -dynamic --make MyBot.hs -O2 -Wall -rtsopts -outputdir dist
ghc -dynamic --make MyBot1.hs -O2 -Wall -rtsopts -outputdir dist1
