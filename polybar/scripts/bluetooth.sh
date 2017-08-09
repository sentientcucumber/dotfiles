#!/bin/bash

status=`systemctl is-active bluetooth.service`

if [ $status == "active" ]; then
    echo ""
else
    echo "%{F#4a4e64}%{F-}"
fi
