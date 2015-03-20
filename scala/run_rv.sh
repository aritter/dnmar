#!/bin/bash

#NOTE: do *NOT* put java  options here!!!  Need to directly edit the sbt script....

#sbt "run --trainProto ../data/train-Multiple.pb.gz --testProto ../data/test-Multiple.pb.gz --outDir experiments/$2 --algorithm $1"
sbt "run --trainProto ../data/train-Multiple.pb.gz --testProto ../data/test-Multiple.pb.gz --outDir experiments_test/$2 --algorithm $1"

#sbt "run --trainProto ../data/train-Multiple.pb.gz --testProto ../data/test-Multiple.pb.gz --outDir $1 --outCompareInfer $1/infer.out"
