#!/bin/bash

printf "Colorado\nConnecticut\nMaryland\nMichigan\nMinnesota\nNew York" > states.names
head -1 data_download_file_reference_2020.csv > IHME.csv
grep -Ff states.names data_download_file_reference_202* >> IHME.csv
