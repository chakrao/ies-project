/*
 * options.h
 *
 *  Created on: Oct 31, 2022
 *      Author: eric
 */

#ifndef OPTIONS_H_
#define OPTIONS_H_

typedef struct {
  char *optname;
  char *parent;
  char *description;
  char *acceptablevalues; //contains a string of acceptable values, and their descriptions (value1=description;value2=descri