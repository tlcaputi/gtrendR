SET /P _inputname= Please enter commit message:
cd "C:/Users/tcapu/Google Drive/modules/gtrendR"
git add .
git commit -m "%_inputname%"
git push
