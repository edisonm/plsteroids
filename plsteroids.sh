#!/bin/bash

swipl -q --tty=false -s `dirname $0`/plsteroids $*
