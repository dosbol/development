#!/usr/bin/env bash

java -jar target/tacas2020.jar no 2 0 clbg/spectral-norm 5500 > /dev/null
java -jar target/tacas2020.jar no 2 0 micro/one-all-one 1
java -jar target/tacas2020.jar no 2 0 micro/one-one-one 1
java -jar target/tacas2020.jar no 2 0 micro/ring 1
java -jar target/tacas2020.jar no 2 0 misc/go-fish > /dev/null
java -jar target/tacas2020.jar no 2 0 misc/chess target/stockfish-mac 60 > /dev/null
java -jar target/tacas2020.jar no 2 0 misc/ttt > /dev/null
java -jar target/tacas2020.jar no 2 0 npb/cg w > /dev/null
java -jar target/tacas2020.jar no 2 0 npb/ft w > /dev/null
java -jar target/tacas2020.jar no 2 0 npb/is w > /dev/null
java -jar target/tacas2020.jar no 2 0 npb/mg w > /dev/null

java -jar target/tacas2020.jar yes 2 0 clbg/spectral-norm 5500 > /dev/null
java -jar target/tacas2020.jar yes 2 0 micro/one-all-one 1
java -jar target/tacas2020.jar yes 2 0 micro/one-one-one 1
java -jar target/tacas2020.jar yes 2 0 micro/ring 1
java -jar target/tacas2020.jar yes 2 0 misc/go-fish > /dev/null
java -jar target/tacas2020.jar yes 2 0 misc/chess target/stockfish-mac 60 > /dev/null
java -jar target/tacas2020.jar yes 2 0 misc/ttt > /dev/null
java -jar target/tacas2020.jar yes 2 0 npb/cg w > /dev/null
java -jar target/tacas2020.jar yes 2 0 npb/ft w > /dev/null
java -jar target/tacas2020.jar yes 2 0 npb/is w > /dev/null
java -jar target/tacas2020.jar yes 2 0 npb/mg w > /dev/null