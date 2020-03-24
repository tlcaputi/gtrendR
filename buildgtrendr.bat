SET /P _inputname= Please enter commit message:
cd "C:/Users/tcapu/Google Drive/modules/gtrendR"
Rscript -e "C:/Users/tcapu/Google Drive/modules/gtrendR/docpackage.R"
git add .
git commit -m "%_inputname%"
git push
pause
