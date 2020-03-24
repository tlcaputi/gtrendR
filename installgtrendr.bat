cd "C:/Users/tcapu/Google Drive"
R -e "devtools::install_github('tlcaputi/gtrendR')"
R CMD INSTALL --build gtrendR_0.0.0.9002.tar.gz
pause
