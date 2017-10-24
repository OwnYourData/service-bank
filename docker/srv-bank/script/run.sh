#!/bin/bash

/opt/bin/entry_point.sh >/dev/null 2>&1 &
{ while ! nc -vz localhost 4444; do sleep 1; done } &> /dev/null

if [ "$#" -ne 1 ]; then
	cat | Rscript /srv-bank/bank.R
else
	Rscript /srv-bank/bank.R $1
fi
