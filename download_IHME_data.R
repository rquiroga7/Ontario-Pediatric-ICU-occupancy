#Get IHME modelled daily infection estimates for states

url="https://ihmecovid19storage.blob.core.windows.net/archive/2022-10-24/data_download_file_reference_2020.csv"
download.file(destfile="data_download_file_reference_2020.csv",url)
url="https://ihmecovid19storage.blob.core.windows.net/archive/2022-10-24/data_download_file_reference_2021.csv"
download.file(destfile="data_download_file_reference_2021.csv",url)
url="https://ihmecovid19storage.blob.core.windows.net/archive/2022-10-24/data_download_file_reference_2022.csv"
download.file(destfile="data_download_file_reference_2022.csv",url)
system2(grep_IHME_data.sh)
