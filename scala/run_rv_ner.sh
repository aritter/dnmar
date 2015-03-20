#!/bin/bash

#NOTE: do *NOT* put java  options here!!!  Need to directly edit the sbt script....

#sbt "run --trainNER ../data/ner/converted --testNER ../data/ner/converted --outDir experiments/$2 --algorithm $1"
sbt "run --trainNER ../data/ner/converted/train --testNER ../data/ner/converted/test --outDir experiments/$2 --algorithm $1"
#sbt "run --trainNER ../data/ner/converted/train --testNER ../data/ner/converted/test --outDir experiments/$2 --algorithm $1"
#sbt "run --trainProto ../data/train-Multiple.pb.gz --testProto ../data/test-Multiple.pb.gz --outDir $1 --outCompareInfer $1/infer.out"
