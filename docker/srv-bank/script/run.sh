#!/bin/bash

/opt/bin/entry_point.sh >/dev/null 2>&1 &
{ while ! nc -vz localhost 4444; do sleep 1; done } &> /dev/null
Rscript /srv-bank/autoLoad.R $1