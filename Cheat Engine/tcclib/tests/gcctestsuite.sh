#!/bin/sh

TESTSUITE_PATH=$HOME/gcc/gcc-3.2/gcc/testsuite/gcc.c-torture
TCC="./tcc -B. -I. -DNO_TRAMPOLINES"
rm -f tcc.sum tcc.fail
nb_ok="0"
nb_failed="0"
nb_exe_failed="0"

for src in $TESTSUITE_PATH/compile/*.c ; do
  echo $TCC -o /tmp/tst.o -c $src 
  $TCC -o /tmp/tst.o -c $src >> tcc.fail 2>&1
  if [ "$?" = "0" ] ; then
    result="PASS"
    nb_ok=$(( $nb_ok + 1 ))
  else
    result="FAIL"
    nb_failed=$(( $nb_failed + 1 ))
  fi
  echo "$result: $src"  >> tcc.sum
done

for src in $TESTSUITE_PATH/exec