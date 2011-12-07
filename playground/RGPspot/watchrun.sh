#!/bin/sh
watch -n 60 'qstat; "echo points evaluated:"; wc -l rgp0001.res; echo "----"; tail rgp0001.res'

