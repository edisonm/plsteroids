#!/bin/bash

swipl -q --tty=no -s `dirname $0`/plsteroids $*
